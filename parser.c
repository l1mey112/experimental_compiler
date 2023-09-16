#include "all.h"

#include <ctype.h>
#include <assert.h>
#include <stdio.h>

#include "stb_ds.h"

typedef struct parser_ctx_t parser_ctx_t;
typedef struct parser_value_t parser_value_t;
typedef struct parser_scope_span_t parser_scope_span_t;
typedef enum parser_value_type_t parser_value_type_t;
typedef enum parser_value_inst_flags_t parser_value_inst_flags_t;
typedef struct parser_import_t parser_import_t;

enum parser_value_type_t {
	VAL_INST,
	VAL_SYM,
	VAL_INTEGER_LITERAL,
	// VAL_FLOAT_LITERAL,
};

// TODO: instructions should never be deleted in the parser...
//       should be `SAFE_DISCARD` instead to discard a value if needed.
enum parser_value_inst_flags_t {
	// a = 230     <-- safe to remove duplicate load
	// see: vemit_garbage()
	VAL_INST_FLAGS_RESULT_SAFE_DELETE = 1 << 0,
	// if (a = 20) <-- error: assignment in condition
	// VAL_INST_FLAGS_RESULT_FROM_ASSIGN = 1 << 1,
	// a || b && c <-- error: abiguous precedence
	// VAL_INST_FLAGS_RESULT_NOT_FROM_BRACES = 1 << 2,
};

struct parser_value_t {
	parser_value_type_t kind;
	loc_t loc;
	type_t type; // -1 for none, TYPE_UNKNOWN is something else

	union {
		sym_resolve_t d_sym;
		struct {
			istr_t lit;
			bool negate;
		} d_literal;
		struct {
			pir_rinst_t inst;
			parser_value_inst_flags_t flags;
		} d_inst;
	};
};

struct parser_scope_span_t {
	pir_rlocal_t start;
	pir_rlocal_t end;
};

struct parser_import_t {
	fs_rnode_t module;
	istr_t name; // name of the module, in `import ... as name` as well
};

struct parser_ctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	token_t tok;
	token_t peek;
	parser_value_t es[128]; // expt stack
	u32 es_len;
	parser_scope_span_t ss[128]; // scope stack
	u32 ss_len;
	parser_import_t is[64]; // import stack
	u32 is_len;
	pir_rinst_t last_inst;
	pir_rblock_t last_block;
	pir_proc_t cproc;
	fs_rfile_t file;
	fs_rnode_t module;
	bool has_done_imports;
};

// allocates using alloc_* functions
const char *tok_dbg_str(token_t tok) {
	// handle identifiers

	u8 *p;

	bool requires_quotes = true;
	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .type = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	//                             : "unexpected `x`, expected `+=`"

	if (TOK_HAS_LIT(tok.type) && tok.lit == (istr_t)-1) {
		requires_quotes = false;
	}
	
	if (TOK_HAS_LIT(tok.type) && tok.lit != (istr_t)-1) {
		str = sv_from(tok.lit);
		len = strlen(str);
	}
    #define X(val, lit) \
		else if (val == tok.type) str = lit, len = strlen(lit);
    TOK_X_LIST
    #undef X

	if (requires_quotes) {
		p = alloc_scratch(len + 2 + 1);
		sprintf((char *)p, "`%s`", str);
	} else {
		p = alloc_scratch(len + 1);
		strcpy((char *)p, str);
	}

	return (const char *)p;
}

const char *tok_literal_representation(tok_t tok) {
	switch (tok) {
		#define X(val, lit) \
			case val: return lit;
		TOK_X_LIST
		#undef X
	}
	assert_not_reached();
}

static parser_ctx_t parser_ctx;

/* static bool is_lvalue(pir_rinst_t inst) {
	return parser_ctx.cproc.insts[inst].is_lvalue;
} */

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

static token_t parser_lex_next(void) {
	while (parser_ctx.pc < parser_ctx.pend) {
		u8 ch = *parser_ctx.pc;

		if (isspace(ch)) {
			parser_ctx.pc++;
			if (ch == '\n') {
				parser_ctx.plast_nl = parser_ctx.pc;
				parser_ctx.line_nr++;
			}
			continue;
		}

		if (is_id_begin(ch)) {
			u8 *start = parser_ctx.pc;

			token_t token = {
				.type = TOK_INTEGER,
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};
			
			do {
				parser_ctx.pc++;
			} while (parser_ctx.pc < parser_ctx.pend && is_id(*parser_ctx.pc));

			// get length and id pointer
			u32 len = parser_ctx.pc - start;
			token.loc.len = len;

			// TODO: this should be optimised to a static hash table

			if (0);
			#define X(val, lit) \
				else if (sv_cmp_literal(start, len, lit)) token.type = val;
			TOK_X_KEYWORDS_LIST
			#undef X
			else {
				token.type = TOK_IDENT;
				token.lit = sv_intern(start, len);
			}

			return token;
		} else if (isdigit(ch)) {
			u8 *start = parser_ctx.pc;

			token_t token = {
				.type = TOK_INTEGER,
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};
			
			do {
				parser_ctx.pc++;
			} while (parser_ctx.pc < parser_ctx.pend && isdigit(*parser_ctx.pc));

			// get length and id pointer
			u32 len = parser_ctx.pc - start;

			token.lit = sv_intern(start, len);
			token.loc.len = len;

			return token;
		} else {
			u8 *start = parser_ctx.pc;
			size_t avail = parser_ctx.pend - parser_ctx.pc;

			// the compiler optimiser would be able to optimise and
			// spot locations of compile time known bounds to memcmp
			// ... where it can
			//
			// this isn't perfect, the old switch case impl would be better
			// the macro method reduces complexity on implementation
			// i know well that the compiler will NOT optimise this efficiently

			// TODO: ~~this should be optimised to a static hash table~~
			//       maybe not, it's not that much faster

			token_t token = {
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};

			if (0);
			#define X(val, lit) \
				else if (strlen(lit) <= avail && memcmp(start, lit, strlen(lit)) == 0) { \
					token.type = val; \
					token.loc.len = strlen(lit); \
					parser_ctx.pc += strlen(lit); \
				}
			TOK_X_OPERATOR_LIST
			#undef X
			else {
				err_with_pos(token.loc, "unexpected character `%c`", ch);
			}

			return token;
		}
	}

	return (token_t){.type = TOK_EOF};
}

static void parser_next(void) {
	parser_ctx.tok = parser_ctx.peek;
	parser_ctx.peek = parser_lex_next();
}

#define DEFAULT_DBG_TOK(expected) (token_t){.type = expected, .lit = (istr_t)-1}

static void parser_expect(tok_t expected) {
	if (parser_ctx.tok.type == expected) {
		parser_next();
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

static void __attribute__ ((__noreturn__)) parser_unexpected(const char *err) {
	err_with_pos(parser_ctx.tok.loc, "unexpected %s, %s", tok_dbg_str(parser_ctx.tok), err);
}

#undef DEFAULT_DBG_TOK

// will be filled from main()
istr_t typeinfo_concrete_istr[_TYPE_CONCRETE_MAX];
u32 typeinfo_concrete_istr_size;

type_t parser_get_type(istr_t lit) {
	for (u32 i = 0; i < typeinfo_concrete_istr_size; i++) {
		if (typeinfo_concrete_istr[i] == lit) {
			return (type_t)i;
		}
	}
	return type_new((typeinfo_t){
		.kind = TYPE_UNKNOWN,
		.d_unknown.lit = lit,
	});
}

type_t parser_parse_type() {
	type_t type;

	switch (parser_ctx.tok.type) {
		case TOK_MUL:
			parser_next();
			return type_new_inc_mul(parser_parse_type());
		case TOK_IDENT:
			// terminating condition
			type = parser_get_type(parser_ctx.tok.lit);
			break;
		default:
			parser_unexpected("expected type definition");
	}

	parser_next();

	return type;
}

pir_rinst_t parser_new_block() {
	// TODO: last_block
	pir_rblock_t id = arrlenu(parser_ctx.cproc.blocks);
	pir_block_t block = {
		.id = id,
		.first = arrlenu(parser_ctx.cproc.insts),
	};
	arrpush(parser_ctx.cproc.blocks, block);
	parser_ctx.last_block = id;
	return id;
}

pir_rinst_t parser_new_inst(pir_inst_t inst) {
	// TODO: last_inst
	pir_rinst_t id = arrlenu(parser_ctx.cproc.insts);
	inst.id = id;
	arrpush(parser_ctx.cproc.insts, inst);
	parser_ctx.cproc.blocks[parser_ctx.last_block].len++;
	return id;
}

enum {
	PREC_UNKOWN,  // default
	PREC_ASSIGN,  // = += -= *= /= %=
	PREC_CMP,     // && || (TODO: needs parens)
	PREC_EQ,      // == != < > <= >=
	PREC_BOR,     // |
	PREC_XOR,     // ^
	PREC_BAND,    // &
	PREC_ADD,     // + -
	PREC_MUL,     // * / %
	PREC_AS_CAST, // as
	PREC_PREFIX,  // - * ! &
	PREC_POSTFIX, // ++ --
	PREC_CALL,    // .x x() x[]
};

static u8 parser_tok_precedence(tok_t type) {
	// PREC_PREFIX is determined elsewhere

	switch (type) {
		case TOK_DOT:
		case TOK_OSQ:
		case TOK_OPAR:
			return PREC_CALL;
		case TOK_INC:
		case TOK_DEC:
			return PREC_POSTFIX;
		case TOK_AS:
			return PREC_AS_CAST;
		case TOK_MUL:
		case TOK_DIV:
		case TOK_MOD:
			return PREC_MUL;
		case TOK_ADD:
		case TOK_SUB:
			return PREC_ADD;
		case TOK_BAND:
			return PREC_BAND;
		case TOK_XOR:
			return PREC_XOR;
		case TOK_BOR:
			return PREC_BOR;
		case TOK_EQ:
		case TOK_NEQ:
		case TOK_LT:
		case TOK_GT:
		case TOK_GE:
		case TOK_LE:
			return PREC_EQ;
		case TOK_AND:
		case TOK_OR:
			return PREC_CMP;
		case TOK_ASSIGN:
		case TOK_ASSIGN_ADD:
		case TOK_ASSIGN_SUB:
		case TOK_ASSIGN_MUL:
		case TOK_ASSIGN_DIV:
		case TOK_ASSIGN_MOD:
			return PREC_ASSIGN;
		default:
			return 0;
	}
}

pir_rlocal_t parser_locate_local(istr_t ident) {
	for (pir_rlocal_t i = parser_ctx.ss_len; i > 0;) {
		i--;
		parser_scope_span_t span = parser_ctx.ss[i];
		for (pir_rlocal_t j = span.end; j > span.start;) {
			j--;
			pir_local_t local = parser_ctx.cproc.locals[j];
			if (local.name == ident) {
				return j;
			}
		}
	}
	return -1;
};

void parser_new_local(pir_local_t local) {
	// make sure var doesn't already exist
	pir_rlocal_t rlocal = parser_locate_local(local.name);
	if (rlocal != (pir_rlocal_t)-1) {
		err_with_pos(local.name_loc, "redefinition of variable `%s` in same scope", sv_from(local.name));
		// err_with_pos(parser_ctx.cproc.locals[rlocal].loc, "previous definition was here");
	}
	
	parser_ctx.ss[parser_ctx.ss_len - 1].end++;
	arrpush(parser_ctx.cproc.locals, local);
	
	// TODO: should be fine to remove...
	assert(parser_ctx.ss[parser_ctx.ss_len - 1].end == arrlenu(parser_ctx.cproc.locals));
}

void parser_push_scope() {
	if (parser_ctx.ss_len >= ARRAYLEN(parser_ctx.ss)) {
		err_without_pos("scope stack is full, stop indenting above 128 levels");
	}
	parser_ctx.ss[parser_ctx.ss_len++] = (parser_scope_span_t){
		.start = arrlenu(parser_ctx.cproc.locals),
		.end = arrlenu(parser_ctx.cproc.locals),
	};
}

void parser_pop_scope() {
	assert(parser_ctx.ss_len > 0);
	parser_ctx.ss_len--;
}

parser_value_t vpop() {
	assert(parser_ctx.es_len > 0);
	return parser_ctx.es[--parser_ctx.es_len];
}

parser_value_t vpop_bottom() {
	assert(parser_ctx.es_len == 1);
	return parser_ctx.es[--parser_ctx.es_len];
}

pir_rinst_t vstore(pir_rinst_t dest, pir_rinst_t src, loc_t loc) {
	return parser_new_inst((pir_inst_t){
		.kind = PIR_LSTORE,
		.loc = loc,
		.type = TYPE_VOID,
		.d_store.dest = dest,
		.d_store.src = src,
	});
}

pir_rinst_t vemit(parser_value_t value) {
	// if the `value` is already an instruction, just return it
	// we have to emit everything else (constants, etc.)

	if (value.kind == VAL_INST) {
		return value.d_inst.inst;
	}

	switch (value.kind) {
		case VAL_SYM: {
			return parser_new_inst((pir_inst_t){
				.kind = PIR_SYM,
				.loc = value.loc,
				.type = value.type,
				.d_sym = value.d_sym,
			});
		}
		case VAL_INTEGER_LITERAL:
			return parser_new_inst((pir_inst_t){
				.kind = PIR_INTEGER_LITERAL,
				.loc = value.loc,
				.type = value.type,
				.d_literal.lit = value.d_literal.lit,
				.d_literal.negate = value.d_literal.negate,
			});
		default:
			assert_not_reached();
	}
}

// UB if the current block does not store the instruction in `inst`
void vdelete_inst(pir_rinst_t inst) {
	// remove from top
	if (inst + 1 == arrlenu(parser_ctx.cproc.insts)) {
		arrpop(parser_ctx.cproc.insts);
		parser_ctx.cproc.blocks[parser_ctx.last_block].len--;
		return;
	}
	// TODO: should this even be reached?
	//       in most cases we should be able to just pop the top
	//       in other cases, it isn't really the parsers job...
	// assert(0 && "TODO: implement NOPs");
	eprintf("nop reached: %u\n", inst);
	parser_ctx.cproc.insts[inst].type = PIR_NOP;
}

void vemit_garbage(parser_value_t value) {
	// remember the use of `VAL_INST_FLAGS_RESULT_SAFE_DELETE` should be done to
	// remove instructions that are essentially, junk.
	// take a v++ expression:
	// 
	//     %0 = sym
	//     %1 = %0 + 1
	//          store(%0, %1)
	//     << %0
	//
	// it's okay to pop off `%0` from the stack and ignore it.
	// extra loads weren't generated for no reason.

	assert(!(value.d_inst.flags & VAL_INST_FLAGS_RESULT_SAFE_DELETE) && "DEPRCATED: ensure you have a very very good reason to use this");

	if (value.kind == VAL_INST && value.d_inst.flags & VAL_INST_FLAGS_RESULT_SAFE_DELETE) {
		// we won't emit, but the load (assign expressions) has already taken place...
		vdelete_inst(value.d_inst.inst);
		return;
	}

	(void)vemit(value);
}

void vpush_ilit(istr_t lit, loc_t loc, bool negate) {
	assert(parser_ctx.es_len < ARRAYLEN(parser_ctx.es));

	// TODO: regarding `TYPE_UNRESOLVED` here:
	//       should we introduce a new type `TYPE_INTERGER_LITERAL` ?
	//       infer or not? that's the question

	parser_ctx.es[parser_ctx.es_len++] = (parser_value_t){
		.kind = VAL_INTEGER_LITERAL,
		.d_literal.lit = lit,
		.d_literal.negate = negate,
		.loc = loc,
		.type = TYPE_UNRESOLVED,
	};
}

void vpush_id(istr_t ident, fs_rnode_t module_ref, loc_t loc) {
	assert(parser_ctx.es_len < ARRAYLEN(parser_ctx.es));

	pir_rlocal_t rlocal = parser_locate_local(ident);
	bool resolved = rlocal != (pir_rlocal_t)-1;

	parser_value_t val;
	type_t type = rlocal == (pir_rlocal_t)-1 ? TYPE_UNRESOLVED : parser_ctx.cproc.locals[rlocal].type;

	module_ref = module_ref == (fs_rnode_t)-1 ? parser_ctx.module : module_ref;

	if (resolved) {
		val = (parser_value_t){
			.kind = VAL_INST,
			.loc = loc,
			.type = type,
			.d_inst.inst = parser_ctx.cproc.locals[rlocal].inst, // TODO: ---------- HERE
		};
	} else {
		val = (parser_value_t){
			.kind = VAL_SYM,
			.loc = loc,
			.type = type,
			.d_sym.sym = SYM_UNRESOLVED,
			.d_sym.d_unresolved.module = module_ref,
			.d_sym.d_unresolved.lit = ident,
		};
	}

	parser_ctx.es[parser_ctx.es_len++] = val;
}

void vpush_inst_flags(pir_rinst_t inst, loc_t loc, parser_value_inst_flags_t flags) {
	assert(parser_ctx.es_len < ARRAYLEN(parser_ctx.es));
	parser_ctx.es[parser_ctx.es_len++] = (parser_value_t){
		.kind = VAL_INST,
		.loc = loc,
		.type = parser_ctx.cproc.insts[inst].type,
		.d_inst.inst = inst,
		.d_inst.flags = flags,
	};
}

// vpop() then vpush()
void vpush_inst_back(pir_rinst_t inst) {
	vpush_inst_flags(inst, parser_ctx.cproc.insts[inst].loc, 0);
}

// vpop() then vpush()
void vpush_inst_back_flags(pir_rinst_t inst, parser_value_inst_flags_t flags) {
	vpush_inst_flags(inst, parser_ctx.cproc.insts[inst].loc, flags);
}

void vpush_inst(pir_rinst_t inst, loc_t loc) {
	vpush_inst_flags(inst, loc, 0);
}

void vinfix(tok_t tok, loc_t loc) {
	assert(parser_ctx.es_len >= 2);

	parser_value_t rhs = vpop();
	parser_value_t lhs = vpop();

	// TODO: `ir_is_lvalue()`
	//       we won't always have access to the concrete representation
	// TODO: should we store lvalue paths as a VAL_* ? it would simplify things
	/* if (tok == TOK_ASSIGN && lhs.kind != VAL_SYM) {
		err_with_pos(loc, "lhs of assignment must be an lvalue");
		return;
	} */

	// TODO: if boolean expression like `||` or `&&`,
	//       type must be TYPE_BOOL.

	// TODO: resolve `TYPE_UNRESOLVED` if possible
	//       checker should expose function that implements this
	// 	 e.g.:
	//       i64 + u32 -> i64
	//       i64 + i32 -> i64

	pir_rinst_t inst;

	pir_rinst_t ilhs = vemit(lhs);
	pir_rinst_t irhs = vemit(rhs);

	if (tok != TOK_ASSIGN) {
		
		inst = parser_new_inst((pir_inst_t){
			.loc = loc,
			.type = TYPE_UNRESOLVED,
			.kind = PIR_INFIX,
			.d_infix.op = tok,
			.d_infix.lhs = ilhs,
			.d_infix.rhs = irhs,
		});

		vpush_inst(inst, loc);
	} else {
		// assign expressions do actually return the value
		// but must be reloaded for use in further expressions
		vstore(ilhs, irhs, loc);
		vpush_inst(irhs, loc);
	}
}

void vprefix(tok_t tok, loc_t loc) {
	assert(parser_ctx.es_len >= 1);

	parser_value_t val = vpop();

	pir_rinst_t ival = vemit(val);

	// TODO: resolve `TYPE_UNRESOLVED` if possible
	//       ....

	pir_rinst_t inst = parser_new_inst((pir_inst_t){
		.loc = loc,
		.type = TYPE_UNRESOLVED,
		.kind = PIR_PREFIX,
		.d_prefix.op = tok,
		.d_prefix.val = ival,
	});

	vpush_inst(inst, loc);
}

void vcast(type_t type, loc_t loc) {
	assert(parser_ctx.es_len >= 1);

	parser_value_t val = vpop();

	(void)type;
	(void)loc;
	(void)val;

	assert(0 && "TODO: emit");
}

// -1 for not found
int parser_import_ident(istr_t name) {
	for (u32 i = 0; i < parser_ctx.is_len; i++) {
		if (parser_ctx.is[i].name == name) {
			return i;
		}
	}
	return -1;
}

void parser_parse_ident(void) {
	loc_t loc = parser_ctx.tok.loc;

	// is imported ident
	int id;
	if ((id = parser_import_ident(parser_ctx.tok.lit)) != -1) {
		parser_next();
		parser_expect(TOK_DOT);
		istr_t ref = parser_ctx.tok.lit;
		parser_expect(TOK_IDENT); // TODO: istr_t span
		vpush_id(ref, parser_ctx.is[id].module, loc);
	} else {
		vpush_id(parser_ctx.tok.lit, -1, loc);
		parser_next();
	}
}

void parser_expr(u8 prec) {
	switch (parser_ctx.tok.type) {
		case TOK_IDENT:
			parser_parse_ident();
			break;
		case TOK_OPAR:
			parser_next();
			parser_expr(0);
			parser_expect(TOK_CPAR);
			break;
		case TOK_INTEGER:
			vpush_ilit(parser_ctx.tok.lit, parser_ctx.tok.loc, false);
			parser_next();
			break;
		default: {
			tok_t t = parser_ctx.tok.type;
		
			if (TOK_IS_PREFIX(parser_ctx.tok.type)) {
				parser_next();
				if (t == TOK_SUB && parser_ctx.tok.type == TOK_INTEGER) {
					vpush_ilit(parser_ctx.tok.lit, parser_ctx.tok.loc, true);
					parser_next();
				} else {
					parser_expr(PREC_PREFIX);
					vprefix(parser_ctx.tok.type, parser_ctx.tok.loc);
				}
			} else {
				parser_unexpected("expected expression");
			}
			break;
		}
	}

	if (parser_ctx.tok.type == TOK_EOF) {
		return;
	}

	while (prec < parser_tok_precedence(parser_ctx.tok.type)) {
		token_t token = parser_ctx.tok;

		switch (token.type) {
			case TOK_AS:
				parser_next();
				vcast(parser_parse_type(), parser_ctx.tok.loc);
				break;
			case TOK_OPAR: {
				pir_rinst_t *cl = (pir_rinst_t *)alloc_scratch(0);
				u32 cc = 0;

				parser_value_t target_v = vpop();
				pir_rinst_t target = vemit(target_v);

				parser_next();
				while (parser_ctx.tok.type != TOK_CPAR) {
					parser_expr(0);
					if (parser_ctx.tok.type == TOK_COMMA) {
						parser_next();
					} else if (parser_ctx.tok.type != TOK_CPAR) {
						parser_unexpected("expected `,` or `)`");
					}
					cl[cc++] = vemit(vpop());
				}
				parser_next();

				// commit
				(void)alloc_scratch(cc * sizeof(pir_rinst_t));

				pir_rinst_t inst = parser_new_inst((pir_inst_t){
					.kind = PIR_CALL,
					.loc = target_v.loc,
					.type = TYPE_UNKNOWN, // TODO: resolve...
					.d_call.target = target,
					.d_call.ilist = cl,
					.d_call.ilen = cc,
				});
				vpush_inst_back(inst);
				break;
			}
			case TOK_INC:
			case TOK_DEC:
				parser_next();
				parser_value_t sym = vpop();
				pir_rinst_t oval = vemit(sym);

				// TODO: unify these locations... they're all over the place!

				vpush_inst_back(oval);
				vpush_ilit(sv_intern((u8*)"1", 1), token.loc, false);
				vinfix(token.type == TOK_INC ? TOK_ADD : TOK_SUB, token.loc);
				pir_rinst_t result = vemit(vpop());
				// vsym_set(result, sym, token.loc);
				vstore(oval, result, token.loc);

				// %0 = sym
				// %1 = %0 + 1
				//      store(%0, %1)
				// << %0
				vpush_inst_back(oval);
				break;
			default:
				if (TOK_IS_INFIX(token.type)) {
					u8 prec = parser_tok_precedence(token.type);
					parser_next();
					parser_expr(prec);
					vinfix(token.type, token.loc);
				} else {
					goto exit;
				}
				break;
		}
	}
exit:
	return;
}

void parser_stmt() {
	// var dec
	if (parser_ctx.tok.type == TOK_MUT || (parser_ctx.tok.type == TOK_IDENT && parser_ctx.peek.type == TOK_COLON)) {
		bool is_mut = false;
		bool has_init = false;

		if (parser_ctx.tok.type == TOK_MUT) {
			is_mut = true;
			parser_next();
		}
		
		istr_t name = parser_ctx.tok.lit;
		loc_t name_loc = parser_ctx.tok.loc;

		parser_next();
		parser_next();
		// a: T
		//    ^

		loc_t type_loc = parser_ctx.tok.loc;
		type_t type = parser_parse_type();

		// -- i don't want "immutable variables ..." to show up before the "redefinition of ..." error
		// -- it will search for the local a second time in `parser_new_local`, that is okay
		// -- pir_rlocal_t rlocal = parser_locate_local(name);

		if (parser_ctx.tok.type == TOK_ASSIGN) {
			has_init = true;
			parser_next();
			parser_expr(0);
		} else if (!is_mut) {
			err_with_pos(name_loc, "immutable variables must have an initial value");
		}

		pir_rlocal_t id = arrlenu(parser_ctx.cproc.locals);		
		pir_rinst_t inst = parser_new_inst((pir_inst_t){
			.kind = PIR_LOCAL,
			.loc = name_loc,
			.type = type,
			.d_local = id,
		});
		parser_new_local((pir_local_t){
			.name = name,
			.name_loc = name_loc,
			.type = type,
			.type_loc = type_loc,
			.inst = inst,
			.is_mut = is_mut,
		});

		if (has_init) {
			parser_value_t val = vpop_bottom();
			pir_rinst_t ival = vemit(val);
			vstore(inst, ival, name_loc); // TODO: the loc_t should span the whole expression...
		}

		return;
	}

	switch (parser_ctx.tok.type) {
		case TOK_RETURN: {
			loc_t loc = parser_ctx.tok.loc;

			// return expr, expr, expr

			pir_rinst_t *retl = (pir_rinst_t *)alloc_scratch(0);
			u32 retc = 0;

			parser_next();
			do {
				parser_expr(0);
				retl[retc++] = vemit(vpop());
			} while (parser_ctx.tok.type == TOK_COMMA);

			// commit
			(void)alloc_scratch(retc * sizeof(pir_rinst_t));

			parser_new_inst((pir_inst_t){
				.kind = PIR_RETURN,
				.loc = loc,
				.type = TYPE_NORETURN, // TODO: should reveal **OBVIOUS** unreachable code errors inside the parser? probably not...
				.d_return.ilist = retl,
				.d_return.ilen = retc,
			});
			break;
		}			
		default:
			parser_expr(0);

			// see: vemit_garbage
			while (parser_ctx.es_len > 0) {
				vemit_garbage(vpop());
			}
	}

	// TODO: the amount of values on the expression stack
	//       is very much known and deterministic, this
	//       could be removed.
	assert(parser_ctx.es_len == 0);
}

void parser_parse_syn_scope(bool new_scope) {
	if (new_scope) {
		parser_push_scope();
	}
	
	parser_expect(TOK_OBRACE);
	while (parser_ctx.tok.type != TOK_CBRACE) {
		parser_stmt();
	}
	parser_next();

	if (new_scope) {
		parser_pop_scope();
	}
}

void parser_fn_def_stmt() {
	istr_t fn_name;
	loc_t fn_name_loc;

	bool is_pub = false;

	while (1) {
		switch (parser_ctx.tok.type) {
			case TOK_FN:
				parser_next();
				goto fparsed;
			case TOK_PUB:
				if (is_pub) {
					err_with_pos(parser_ctx.tok.loc, "duplicate `pub`");
				}
				is_pub = true;
				parser_next();
				break;
			default:
				// can only be `fn` on entry
				parser_unexpected("expected `fn`");
		}
	}
fparsed:
	fn_name_loc = parser_ctx.tok.loc;
	fn_name = parser_ctx.tok.lit;
	parser_expect(TOK_IDENT);

	parser_ctx.cproc = (pir_proc_t){
		.name = fn_name,
		.name_loc = fn_name_loc,
	};

	parser_ctx.last_inst = (pir_rinst_t)-1;
	parser_ctx.last_block = (pir_rblock_t)-1;

	parser_ctx.cproc.entry = parser_new_block();
	
	// add default scope for arguments
	parser_push_scope();

	// allocate with scratch buffer..
	u32 args = 0;
	type_t *arg_types = (type_t*)alloc_scratch(0);

	parser_expect(TOK_OPAR);
	while (parser_ctx.tok.type != TOK_CPAR) {
		loc_t arg_loc = parser_ctx.tok.loc;
		istr_t arg_name = parser_ctx.tok.lit;
		parser_expect(TOK_IDENT);
		parser_expect(TOK_COLON);
		loc_t type_loc = parser_ctx.tok.loc;
		type_t type = parser_parse_type();
		// TODO: extend the current loc to the end of the type

		pir_local_t arg = {
			.name = arg_name,
			.name_loc = arg_loc,
			.type = type,
			.type_loc = type_loc,
			.is_arg = true,
		};

		for (u32 i = 0; i < args; i++) {
			if (arg.name == parser_ctx.cproc.locals[i].name) {
				err_with_pos(fn_name_loc, "duplicate function parameter name `%s`", sv_from(arg_name));
			}
		}

		arg.inst = parser_new_inst((pir_inst_t){
			.kind = PIR_ARG,
			.loc = arg_loc,
			.type = type,
			.d_local = args,
		});
		// will be added to scope stack
		parser_new_local(arg);
		arg_types[args++] = type; // insert

		if (parser_ctx.tok.type == TOK_COMMA) {
			parser_next();
		} else if (parser_ctx.tok.type != TOK_CPAR) {
			parser_unexpected("expected `,` or `)`");
		}
	}

	// commit: arg_types_loc
	(void)alloc_scratch(args * sizeof(type_t));

	u32 rets = 0;
	type_t *ret_types = (type_t*)alloc_scratch(0);

	// ): i32
	// ): (i32, T)
	parser_next();
	// (a: T, b: T): ...
	//             ^

	if (parser_ctx.tok.type == TOK_COLON) {
		parser_next();
		if (parser_ctx.tok.type == TOK_OPAR) {
			parser_next();
			while (parser_ctx.tok.type != TOK_CPAR) {
				loc_t type_loc = parser_ctx.tok.loc;
				type_t type = parser_parse_type();

				(void)type_loc; // TODO: return type locations aren't used yet

				ret_types[rets++] = type;

				if (parser_ctx.tok.type == TOK_COMMA) {
					parser_next();
				} else if (parser_ctx.tok.type != TOK_CPAR) {
					parser_unexpected("expected `,` or `)`");
				}
			}
			parser_next();
		} else {
			loc_t type_loc = parser_ctx.tok.loc;
			type_t type = parser_parse_type();

			(void)type_loc; // TODO: return type locations aren't used yet

			ret_types[rets++] = type;
		}
	}

	// commit: ret_types_loc
	(void)alloc_scratch(rets * sizeof(type_t));

	parser_ctx.cproc.type = type_new((typeinfo_t){
		.kind = TYPE_FN,
		.d_fn.args = arg_types,
		.d_fn.args_len = args,
		.d_fn.rets = ret_types,
		.d_fn.rets_len = rets,
	});
	parser_ctx.cproc.args = args;
	parser_ctx.cproc.rets = rets;

	// ): (a, b) {}
	//           ^
	// ): (a, b) asm {}
	//           ^^^
	
	if (parser_ctx.tok.type != TOK_EOF) {
		if (parser_ctx.tok.type == TOK_OBRACE) {
			parser_parse_syn_scope(false);
		} else if (parser_ctx.tok.type == TOK_ASM) {
			assert(0 && "TODO: asm unimplemented");
		} else {
			parser_unexpected("expected `{`");
		}
	} else {
		assert(0 && "TODO: needs function body for now");
	}

	parser_pop_scope();
	assert(parser_ctx.ss_len == 0);

	istr_t symbol_name = fs_module_symbol_str(parser_ctx.module, parser_ctx.cproc.name);

	// TODO: duplicate name_loc for proc?
	table_new((sym_t){
		.key = symbol_name,
		.kind = SYM_PROC,
		.module = parser_ctx.module,
		.type = parser_ctx.cproc.type,
		.name_loc = parser_ctx.cproc.name_loc,
		.is_pub = is_pub,
		.proc = parser_ctx.cproc,
	});
}

void parser_top_stmt() {
	if (parser_ctx.tok.type != TOK_IMPORT) {
		parser_ctx.has_done_imports = true;
	}
	
	switch (parser_ctx.tok.type) {
		case TOK_IMPORT:
			if (parser_ctx.has_done_imports) {
				parser_unexpected("import declaration must come before code");
			}

			loc_t path_loc = parser_ctx.tok.loc; // TODO: extend to end of path
			istr_t *fields = (istr_t *)alloc_scratch(0);
			u32 fields_len = 0;
			do {
				parser_next();
				istr_t field = parser_ctx.tok.lit;
				parser_expect(TOK_IDENT);
				fields[fields_len++] = field;
			} while(parser_ctx.tok.type == TOK_DOT);
			// commit
			alloc_scratch(fields_len * sizeof(istr_t));
			{
				fs_rnode_t module = fs_register_import(parser_ctx.module, fields, fields_len, path_loc);
				istr_t module_ident = fields[fields_len - 1];

				// check duplicate import
				for (u32 i = 0; i < parser_ctx.is_len; i++) {
					parser_import_t *is = &parser_ctx.is[i];
					if (is->module == module) {
						err_with_pos(path_loc, "error: duplicate import");
					} else if (is->name == module_ident) {
						err_with_pos(path_loc, "error: import name `%s` already used", sv_from(module_ident));
					}
				}

				// insert import
				assert(parser_ctx.is_len < ARRAYLEN(parser_ctx.is));
				parser_ctx.is[parser_ctx.is_len++] = (parser_import_t){
					.module = module,
					.name = module_ident, // TODO: import ... as ...
				};
			}
			alloc_reset((u8 *)fields);
			break;
		case TOK_FN:
		case TOK_PUB:
			parser_fn_def_stmt();
			break;
		default:
			parser_unexpected("expected toplevel statement");
			break;
	}
}

void parser_parse_file(fs_rfile_t file) {
	fs_file_t *f = fs_filep(file);
	
	parser_ctx = (parser_ctx_t){
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
		.file = file,
		.module = f->module,
	};

	parser_next(); // tok
	parser_next(); // tok peek

	while (parser_ctx.tok.type != TOK_EOF) {
		parser_top_stmt();
	}
}