#include "all.h"

#include <ctype.h>
#include <assert.h>
#include <stdio.h>

#include "stb_ds.h"

typedef struct parser_ctx_t parser_ctx_t;
typedef struct parser_value_t parser_value_t;
typedef struct parser_scope_span_t parser_scope_span_t;
typedef enum parser_value_type_t parser_value_type_t;

enum parser_value_type_t {
	PVAL_INST,
	PVAL_LOCAL,
	PVAL_IDENT,
	PVAL_CONST,
};

struct parser_value_t {
	parser_value_type_t kind;
	loc_t loc;
	type_t type; // -1 for none, TYPE_UNKNOWN is something else

	union {
		struct {
			u32 id;
		} local;
		istr_t lit;
	};
};

struct parser_scope_span_t {
	hir_rlocal start;
	hir_rlocal end;
};

struct parser_ctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	rfile_t file;
	token_t tok;
	token_t peek;
	hir_proc_t cproc;
	parser_value_t es[128];
	u32 es_len;
	parser_scope_span_t ss[128];
	u32 ss_len;
	hir_rinst last_inst;
	hir_rblock last_block;
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

static parser_ctx_t parser_ctx;

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

/* static void parser_check(tok_t expected) {
	if (parser_ctx.tok.type != expected) {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
} */

static void parser_expect(tok_t expected) {
	if (parser_ctx.tok.type == expected) {
		parser_next();
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

static void parser_expect_not_eof() {
	if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF");
	} else {
		parser_next();
	}
}

static void __attribute__ ((__noreturn__)) parser_unexpected(const char *err) {
	err_with_pos(parser_ctx.tok.loc, "unexpected %s, %s", tok_dbg_str(parser_ctx.tok), err);
}

#undef DEFAULT_DBG_TOK

// will be filled from main()
istr_t typeinfo_concrete_istr[_TYPE_CONCRETE_MAX];

type_t parser_get_type(istr_t lit) {
	for (u32 i = 0; i < _TYPE_CONCRETE_MAX; i++) {
		if (typeinfo_concrete_istr[i] == lit) {
			return (type_t)i;
		}
	}
	return table_new((typeinfo_t){
		.kind = TYPE_UNKNOWN,
		.d_unknown.lit = lit,
	});
}

type_t parser_parse_type() {
	type_t type;

	switch (parser_ctx.tok.type) {
		case TOK_MUL:
			parser_next();
			return table_new_inc_mul(parser_parse_type());
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

hir_rinst parser_new_block() {
	// TODO: last_block
	hir_rblock id = arrlenu(parser_ctx.cproc.blocks);
	hir_block_t block = {
		.id = id,
		.first = arrlenu(parser_ctx.cproc.insts),
	};
	arrpush(parser_ctx.cproc.blocks, block);
	parser_ctx.last_block = id;
	return id;
}

hir_rinst parser_new_inst(hir_inst_t inst) {
	// TODO: last_inst
	hir_rinst id = arrlenu(parser_ctx.cproc.insts);
	inst.id = id;
	arrpush(parser_ctx.cproc.insts, inst);
	parser_ctx.cproc.blocks[parser_ctx.last_block].len++;
	return id;
}

void parser_stmt() {}

void parser_new_local(hir_local_t local) {
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

void parser_parse_syn_scope() {
	parser_expect(TOK_OBRACE);
	while (parser_ctx.tok.type != TOK_CBRACE) {
		parser_stmt();
	}
	parser_next();
}

void parser_fn_def_stmt() {
	istr_t fn_name;
	loc_t fn_name_loc;

	bool is_pure = false;
	bool is_extern = false;

	bool first_tok = false;
	while (1) {
		switch (parser_ctx.tok.type) {
			case TOK_FN:
				parser_next();
				goto fparsed;
			case TOK_PURE:
				if (is_pure) {
					err_with_pos(parser_ctx.tok.loc, "duplicate pure");
				}
				is_pure = true;
				parser_next();
				break;
			case TOK_EXTERN:
				if (is_extern) {
					err_with_pos(parser_ctx.tok.loc, "duplicate extern");
				} else if (!first_tok) {
					err_with_pos(parser_ctx.tok.loc, "extern must come first in function definition");
				}
				is_extern = true;
				parser_next();
				break;
			default:
				parser_unexpected("expected `fn`, `pure` or `extern`");
		}
		first_tok = true;
	}
fparsed:
	fn_name_loc = parser_ctx.tok.loc;
	fn_name = parser_ctx.tok.lit;
	parser_expect(TOK_IDENT);

	parser_ctx.cproc = (hir_proc_t){
		.name = fn_name,
		.name_loc = fn_name_loc,
		.is_extern = is_extern,
		.is_pure = is_pure,
	};

	parser_ctx.last_inst = (hir_rinst)-1;
	parser_ctx.last_block = (hir_rblock)-1;

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

		hir_local_t arg = {
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

		arg.inst = parser_new_inst((hir_inst_t){
			.type = HIR_ARG,
			.d_arg.local = args,
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
		parser_expect_not_eof();
		if (parser_ctx.tok.type == TOK_OPAR) {
			parser_expect_not_eof();
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

	parser_ctx.cproc.type = table_new((typeinfo_t){
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
			parser_parse_syn_scope();
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

	dump_proc(&parser_ctx.cproc);
}

void parser_top_stmt() {
	switch (parser_ctx.tok.type) {
		case TOK_FN:
		case TOK_EXTERN:
		case TOK_PURE:
			parser_fn_def_stmt();
			break;
		default:
			parser_unexpected("expected function definition");
			break;
	}
}

void file_parse(rfile_t file) {
	file_entry_t *f = &file_entries[file];
	
	parser_ctx = (parser_ctx_t){
		.file = file,
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
	};

	parser_next(); // tok
	parser_next(); // tok peek

	while (parser_ctx.tok.type != TOK_EOF) {
		parser_top_stmt();
	}
}