#include "all.h"

#include <assert.h>

#include "stb_ds.h"

parser_ctx_t parser_ctx;

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

void parser_new_scope_from(parser_scope_t ps) {
	if (parser_ctx.ss_len >= ARRAYLEN(parser_ctx.ss)) {
		err_without_pos("scope stack is full, stop indenting above 128 levels");
	}
	parser_ctx.ss[parser_ctx.ss_len++] = ps;
}

// `bb` and `pred` are only needed on "continue" and "break" scopes
void parser_new_scope(istr_t label, pir_rblock_t bb, pir_rblock_t pred, parser_scope_kind_t kind) {
	parser_new_scope_from((parser_scope_t){
		.label = label,
		.kind = kind,
		.var_start = arrlenu(parser_ctx.cproc.locals),
		.var_end = arrlenu(parser_ctx.cproc.locals),
		.bb = bb,
		.pred = pred,
	});
}

parser_scope_t parser_pop_scope_into() {
	assert(parser_ctx.ss_len > 0);
	return parser_ctx.ss[--parser_ctx.ss_len];
}

void parser_pop_scope() {
	assert(parser_ctx.ss_len > 0);
	parser_ctx.ss_len--;
}

enum {
	PREC_UNKNOWN, // default
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
			return PREC_UNKNOWN;
	}
}

void parser_expr(pir_rblock_t bb, u8 prec) {
	switch (parser_ctx.tok.type) {
		case TOK_IDENT:
			parser_parse_ident();
			break;
		case TOK_OPAR:
			parser_next();
			parser_expr(bb, 0);
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
					parser_expr(bb, PREC_PREFIX);
					vprefix(bb, parser_ctx.tok.type, parser_ctx.tok.loc);
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
				pir_rinst_t target = vemit(bb, target_v);

				parser_next();
				while (parser_ctx.tok.type != TOK_CPAR) {
					parser_expr(bb, 0);
					if (parser_ctx.tok.type == TOK_COMMA) {
						parser_next();
					} else if (parser_ctx.tok.type != TOK_CPAR) {
						parser_unexpected("expected `,` or `)`");
					}
					cl[cc++] = vemit(bb, vpop());
				}
				parser_next();

				// commit
				(void)alloc_scratch(cc * sizeof(pir_rinst_t));

				pir_rinst_t inst = parser_inew(bb, (pir_inst_t){
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
				pir_rinst_t oval = vemit(bb, sym);

				// TODO: unify these locations... they're all over the place!

				vpush_inst_back(oval);
				vpush_ilit(sv_intern((u8*)"1", 1), token.loc, false);
				vinfix(bb, token.type == TOK_INC ? TOK_ADD : TOK_SUB, token.loc);
				pir_rinst_t result = vemit(bb, vpop());
				// vsym_set(result, sym, token.loc);
				vstore(bb, sym, result, token.loc);

				// %0 = sym
				// %1 = %0 + 1
				//      store(sym, %1)
				// << %0
				vpush_inst_back(oval);
				break;
			default:
				if (TOK_IS_INFIX(token.type)) {
					u8 prec = parser_tok_precedence(token.type);
					parser_next();
					parser_expr(bb, prec);
					vinfix(bb, token.type, token.loc);
				} else {
					goto exit;
				}
				break;
		}
	}
exit:
	return;
}

u32 parser_search_loop_scope() {
	for (u32 i = parser_ctx.ss_len; i--;) {
		if (parser_ctx.ss[i].kind == SCOPE_LOOP) {
			return i;
		}
	}
	return (u32)-1;
}

u32 parser_search_label(istr_t label_name) {
	// search backwards, even though labels can't be shadowed
	for (u32 i = parser_ctx.ss_len; i--;) {
		if (parser_ctx.ss[i].label == label_name) {
			return i;
		}
	}
	return (u32)-1;
}

pir_rblock_t parser_stmt(pir_rblock_t bb);

pir_rblock_t parser_block(pir_rblock_t entry) {
	parser_expect(TOK_OBRACE);
	while (parser_ctx.tok.type != TOK_CBRACE) {
		entry = parser_stmt(entry);
	}
	parser_next();
	return entry;
}

pir_rblock_t parser_if_stmt(istr_t label_name, pir_rblock_t entry) {
	bool has_entered = false;
	bool has_else = false;

	pir_rblock_t pred = pir_new_block(&parser_ctx.cproc);

	if (label_name != (istr_t)-1) {
		pir_rblock_t next = pir_new_block(&parser_ctx.cproc);
		parser_inew(entry, (pir_inst_t){
			.kind = PIR_JMP,
			.loc = parser_ctx.tok.loc,
			.type = TYPE_NORETURN,
			.d_jmp = next,
		});
		entry = next;
	}

	pir_rblock_t arm_entry = entry;
	pir_rblock_t on_false;
	while (!has_else) {
		switch (parser_ctx.tok.type) {
			case TOK_IF:
				if (has_entered) {
					// if (expr) {} if (expr) {}
					goto done;
				}
				has_entered = true;
				parser_next();
				break;
			case TOK_ELSE:
				parser_next();
				arm_entry = on_false;
				if (parser_ctx.tok.type == TOK_IF) {
					parser_next();
					break;
				}
				has_else = true;
				// else {}
				//        ^>
				break;
			default:
				goto done;
		}

		pir_rblock_t on_true;

		if (!has_else) {
			on_true = pir_new_block(&parser_ctx.cproc);
			
			// (expr) {}
			// ^
			parser_expect(TOK_OPAR);
			parser_expr(arm_entry, 0);
			parser_expect(TOK_CPAR);

			pir_rinst_t cond = vemit(arm_entry, vpop_bottom()); 

			on_false = pir_new_block(&parser_ctx.cproc);

			parser_inew(arm_entry, (pir_inst_t){
				.kind = PIR_IF,
				.type = TYPE_NORETURN,
				.d_if.cond = cond,
				.d_if.on_true = on_true,
				.d_if.on_false = on_false,
			});
		} else {
			on_true = arm_entry;
			// on_false = arm_entry;
		}

		parser_new_scope(label_name, entry, pred, SCOPE_BLOCK);
		{
			on_true = parser_block(on_true);
		}
		parser_pop_scope();

		parser_inew(on_true, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = pred,
		});
	}
done:;

	if (!has_else) {
		parser_inew(on_false, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = pred,
		});
	}

	return pred;
}

pir_rblock_t parser_open_block(istr_t label_name, pir_rblock_t entry) {
	// label avail, may be jumped to
	// TODO: give basic blocks debug names? pass it the label_name

	pir_rblock_t pred = (pir_rblock_t)-1;

	// entry:
	//         ...
	// next:
	//         continue :next
	//         break :pred
	// pred:
	//
	if (label_name != (istr_t)-1) {
		pir_rblock_t next = pir_new_block(&parser_ctx.cproc);
		parser_inew(entry, (pir_inst_t){
			.kind = PIR_JMP,
			.loc = parser_ctx.tok.loc,
			.type = TYPE_NORETURN,
			.d_jmp = next,
		});
		entry = next;
		pred = pir_new_block(&parser_ctx.cproc);
	}

	parser_new_scope(label_name, entry, pred, SCOPE_BLOCK);
	{
		entry = parser_block(entry);
	}
	parser_pop_scope();

	// link up last entry with the pred if needed
	if (pred != (pir_rblock_t)-1) {
		parser_inew(entry, (pir_inst_t){
			.kind = PIR_JMP,
			.loc = parser_ctx.tok.loc,
			.type = TYPE_NORETURN,
			.d_jmp = pred,
		});
		return pred;
	} else {
		return entry;
	}
}

void parser_break_continue(pir_rblock_t bb) {
	bool is_break = parser_ctx.tok.type == TOK_BREAK;
	loc_t key_loc = parser_ctx.tok.loc;

	pir_rblock_t jmp_target;
	u32 scope;

	parser_next();
	if (parser_ctx.tok.type == TOK_COLON) {
		parser_next();
		loc_t loc = parser_ctx.tok.loc;
		istr_t label = parser_ctx.tok.lit;
		parser_expect(TOK_IDENT);
		if ((scope = parser_search_label(label)) == (u32)-1) {
			err_with_pos(loc, "label `%s` not found", sv_from(label));
		}
	} else {
		if ((scope = parser_search_loop_scope()) == (u32)-1) {
			err_with_pos(key_loc, "no enclosing loop");
		}
	}

	jmp_target = is_break ? parser_ctx.ss[scope].pred : parser_ctx.ss[scope].bb;

	// emit
	parser_inew(bb, (pir_inst_t){
		.kind = PIR_JMP,
		.loc = key_loc,
		.type = TYPE_NORETURN,
		.d_jmp = jmp_target,
	});
}

// while () : () {}
// while () {}
// while {}
pir_rblock_t parser_while(istr_t label_name, pir_rblock_t bb) {
	// cond:
	//     cond()
	// body:
	//     body()
	// inc:
	//     inc()
	// end:

	// while () : () {}
	pir_rblock_t cond = (pir_rblock_t)-1;
	pir_rinst_t cond_inst = (pir_rinst_t)-1;
	pir_rblock_t inc = (pir_rblock_t)-1;

	parser_next();
	// while ()
	//       ^
	if (parser_ctx.tok.type == TOK_OPAR) {
		parser_next();
		cond = pir_new_block(&parser_ctx.cproc);
		parser_expr(cond, 0);
		parser_expect(TOK_CPAR);

		cond_inst = vemit(cond, vpop_bottom());

		parser_inew(bb, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = cond,
		});
	}

	// while () : () {}
	//          ^
	if (parser_ctx.tok.type == TOK_COLON && cond != (pir_rblock_t)-1) {
		inc = pir_new_block(&parser_ctx.cproc);
		parser_next();
		parser_expect(TOK_OPAR);
		parser_expr(inc, 0);
		parser_expect(TOK_CPAR);
	}

	// while () : () {}
	//               ^

	pir_rblock_t pred = pir_new_block(&parser_ctx.cproc);
	pir_rblock_t body = pir_new_block(&parser_ctx.cproc);

	if (cond == (pir_rblock_t)-1) {
		parser_inew(bb, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = body,
		});
	} else {
		parser_inew(cond, (pir_inst_t){
			.kind = PIR_IF,
			.type = TYPE_NORETURN,
			.d_if.cond = cond_inst,
			.d_if.on_true = body,
			.d_if.on_false = pred,
		});
		if (inc != (pir_rblock_t)-1) {
			parser_inew(inc, (pir_inst_t){
				.kind = PIR_JMP,
				.type = TYPE_NORETURN,
				.d_jmp = cond,
			});
		}
	}
	
	pir_rblock_t continue_target = cond != (pir_rblock_t)-1 ? cond : body;

	pir_rblock_t body_bottom = body;
	parser_new_scope(label_name, continue_target, pred, SCOPE_LOOP);
	{
		body_bottom = parser_block(body_bottom);
	}
	parser_pop_scope();

	// body -> jmp cond
	// body -> jmp inc -> jmp cond
	// body -> jmp body

	// inc implies cond
	if (inc != (pir_rblock_t)-1) {
		parser_inew(body_bottom, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = inc,
		});
	} else if (cond != (pir_rblock_t)-1) {
		parser_inew(body_bottom, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = cond,
		});
	} else {
		parser_inew(body_bottom, (pir_inst_t){
			.kind = PIR_JMP,
			.type = TYPE_NORETURN,
			.d_jmp = body,
		});
	}

	return pred;
}

// let v: T = expr
// mut v: T = expr
void parser_var_decl(pir_rblock_t bb) {
	bool is_mut = false;
	bool has_init = false;
	if (parser_ctx.tok.type == TOK_MUT) {
		is_mut = true;
	}
	parser_next();

	istr_t name = parser_ctx.tok.lit;
	loc_t name_loc = parser_ctx.tok.loc;

	parser_expect(TOK_IDENT);
	parser_expect(TOK_COLON);
	// let a: T
	//        ^

	loc_t type_loc = parser_ctx.tok.loc;
	type_t type = parser_parse_type();

	if (parser_ctx.tok.type == TOK_ASSIGN) {
		has_init = true;
		parser_next();
		parser_expr(bb, 0);
	} else if (!is_mut) {
		err_with_pos(name_loc, "immutable variables must have an initial value");
	}

	pir_rlocal_t local = parser_new_local((pir_local_t){
		.name = name,
		.name_loc = name_loc,
		.type = type,
		.type_loc = type_loc,
		.is_mut = is_mut,
	});

	if (has_init) {
		parser_value_t val = vpop_bottom();
		pir_rinst_t ival = vemit(bb, val);
		vstore_local(bb, local, ival, name_loc); // TODO: the loc_t should span the whole expression...
	}
}

pir_rblock_t parser_stmt(pir_rblock_t bb) {
	// let v: T = expr
	// mut v: T = expr
	if (parser_ctx.tok.type == TOK_MUT || parser_ctx.tok.type == TOK_LET) {
		parser_var_decl(bb);
		return bb;
	}

	istr_t label_name = (istr_t)-1;
	loc_t label_loc = parser_ctx.tok.loc;

	// label: {}
	if (parser_ctx.tok.type == TOK_IDENT && parser_ctx.peek.type == TOK_COLON) {
		label_name = parser_ctx.tok.lit;
		if (parser_search_label(label_name) != (u32)-1) {
			err_with_pos(parser_ctx.tok.loc, "label `%s` shadows existing", sv_from(label_name));
		}
		parser_next();
		parser_next();
	}

	switch (parser_ctx.tok.type) {
		case TOK_OBRACE: {
			return parser_open_block(label_name, bb);
		}
		case TOK_IF: {
			return parser_if_stmt(label_name, bb);
		}
		case TOK_WHILE: {
			return parser_while(label_name, bb);
		}
		case TOK_FOR: {}
			assert_not_reached();
			return bb;
		default:
			break;
	}

	if (label_name != (istr_t)-1) {
		err_with_pos(label_loc, "label `%s` is not part of a block", sv_from(label_name));
	}

	switch (parser_ctx.tok.type) {
		case TOK_BREAK:
		case TOK_CONTINUE: {
			parser_break_continue(bb);
			return bb;
		}
		case TOK_RETURN: {
			loc_t loc = parser_ctx.tok.loc;

			pir_rinst_t *retl = (pir_rinst_t *)alloc_scratch(0);
			u32 retc = 0;

			parser_next();
			do {
				parser_expr(bb, 0);
				retl[retc++] = vemit(bb, vpop());
			} while (parser_ctx.tok.type == TOK_COMMA);

			// commit
			// lets just hope expr() doesn't allocate too
			(void)alloc_scratch(retc * sizeof(pir_rinst_t));

			parser_inew(bb, (pir_inst_t){
				.kind = PIR_RETURN,
				.loc = loc,
				.type = TYPE_NORETURN, // TODO: should reveal **OBVIOUS** unreachable code errors inside the parser? probably not...
				.d_return.ilist = retl,
				.d_return.ilen = retc,
			});
			return bb;
		}
		default:
			break;
	}

	parser_expr(bb, 0);
	// flush contents
	while (parser_ctx.es_len > 0) {
		vemit(bb, vpop());
	}

	return bb;
}

void parser_function() {
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

	// add default scope for arguments
	parser_new_scope(-1, -1, -1, SCOPE_FUNCTION);

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

	// TODO: support tuples, you know you want to..
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
			pir_rblock_t entry = pir_new_block(&parser_ctx.cproc);
			parser_check(TOK_OBRACE);
			pir_rblock_t fall_through = parser_open_block(-1, entry);
			(void)fall_through;
			// TODO: fall_through is the block that the natural flow of execution reaches in a function
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

void parser_import() {
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
}

void parser_top_stmt() {
	if (parser_ctx.tok.type != TOK_IMPORT) {
		parser_ctx.has_done_imports = true;
	}
	
	switch (parser_ctx.tok.type) {
		case TOK_IMPORT:
			parser_import();
			break;
		case TOK_FN:
		case TOK_PUB:
			parser_function();
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