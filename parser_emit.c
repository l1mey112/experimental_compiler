#include "all.h"

#include "stb_ds.h"

// may be invalidated
pir_proc_t *parser_current_proc(void) {
	if (parser_ctx.is_toplevel) {
		return &table[parser_ctx.init].proc;
	} else {
		return &parser_ctx.cproc;
	}
}

pir_rblock_t parser_bnew(void) {
	pir_proc_t *pir_proc = parser_current_proc();
	return pir_new_block(pir_proc);
}

pir_rinst_t parser_inew(pir_rblock_t bb, pir_inst_t inst) {
	pir_proc_t *pir_proc = parser_current_proc();
	return pir_insert(pir_proc, bb, inst);
}

pir_rinst_t vstore_local(pir_rblock_t bb, pir_rlocal_t dest, pir_rinst_t src, loc_t loc) {
	return parser_inew(bb, (pir_inst_t){
		.kind = PIR_LSTORE,
		.loc = loc,
		.type = TYPE_VOID,
		.d_store.local = dest,
		.d_store.src = src,
		.d_store.is_sym = false,
	});
}

pir_rinst_t vstore_sym(pir_rblock_t bb, sym_resolve_t dest, pir_rinst_t src, loc_t loc) {
	return parser_inew(bb, (pir_inst_t){
		.kind = PIR_LSTORE,
		.loc = loc,
		.type = TYPE_VOID,
		.d_store.sym = dest,
		.d_store.src = src,
		.d_store.is_sym = true,
	});
}

pir_rinst_t vstore(pir_rblock_t bb, parser_value_t dest, pir_rinst_t src, loc_t loc) {
	switch (dest.kind) {
		case VAL_SYM:
			return vstore_sym(bb, dest.d_sym, src, loc);
		case VAL_LOCAL:
			return vstore_local(bb, dest.d_local, src, loc);
		default:
			err_with_pos(dest.loc, "cannot assign to this");
	}
}

parser_value_t vpop() {
	assert(parser_ctx.es_len > 0);
	return parser_ctx.es[--parser_ctx.es_len];
}

parser_value_t vpop_bottom() {
	assert(parser_ctx.es_len == 1);
	return parser_ctx.es[--parser_ctx.es_len];
}

pir_rinst_t vemit(pir_rblock_t bb, parser_value_t value) {
	// if the `value` is already an instruction, just return it
	// we have to emit everything else (constants, etc.)

	if (value.kind == VAL_INST) {
		return value.d_inst;
	}

	switch (value.kind) {
		case VAL_SYM: {
			return parser_inew(bb, (pir_inst_t){
				.kind = PIR_LLOAD,
				.loc = value.loc,
				.type = value.type,
				.d_load.is_sym = true,
				.d_load.sym = value.d_sym,
			});
		}
		case VAL_LOCAL: {
			return parser_inew(bb, (pir_inst_t){
				.kind = PIR_LLOAD,
				.loc = value.loc,
				.type = value.type,
				.d_load.is_sym = false,
				.d_load.local = value.d_local,
			});
		}
		case VAL_INTEGER_LITERAL:
			return parser_inew(bb, (pir_inst_t){
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

pir_rlocal_t parser_locate_local(istr_t ident) {
	pir_proc_t *procp = parser_current_proc();

	for (pir_rlocal_t i = parser_ctx.ss_len; i > 0;) {
		i--;
		parser_scope_t span = parser_ctx.ss[i];
		for (pir_rlocal_t j = span.var_end; j > span.var_start;) {
			j--;
			pir_local_t local = procp->locals[j];
			if (local.name == ident) {
				return j;
			}
		}
	}
	return -1;
};

pir_rlocal_t parser_new_local(pir_local_t local) {
	pir_proc_t *procp = parser_current_proc();

	// make sure var doesn't already exist
	pir_rlocal_t rlocal = parser_locate_local(local.name);
	if (rlocal != (pir_rlocal_t)-1) {
		if (procp->locals[rlocal].is_arg) {
			err_with_pos(local.name_loc, "redefinition of argument `%s`", sv_from(local.name));
		} else {
			err_with_pos(local.name_loc, "redefinition of variable `%s` in same scope", sv_from(local.name));
		}
		// err_with_pos(procp->locals[rlocal].loc, "previous definition was here");
	} else {
		rlocal = arrlenu(procp->locals);
	}

	parser_ctx.ss[parser_ctx.ss_len - 1].var_end++;
	arrpush(procp->locals, local);

	// TODO: should be fine to remove...
	assert(parser_ctx.ss[parser_ctx.ss_len - 1].var_end == arrlenu(procp->locals));

	return rlocal;
}

void vpush_id(istr_t ident, fs_rnode_t module_ref, loc_t loc) {
	pir_proc_t *procp = parser_current_proc();
	assert(parser_ctx.es_len < ARRAYLEN(parser_ctx.es));

	pir_rlocal_t rlocal = parser_locate_local(ident);
	bool resolved = rlocal != (pir_rlocal_t)-1;

	parser_value_t val;
	type_t type = rlocal == (pir_rlocal_t)-1 ? TYPE_UNRESOLVED : procp->locals[rlocal].type;

	module_ref = module_ref == (fs_rnode_t)-1 ? parser_ctx.module : module_ref;

	if (resolved) {
		val = (parser_value_t){
			.kind = VAL_LOCAL,
			.loc = loc,
			.type = type,
			.d_local = rlocal,
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

void vpush_inst(pir_rinst_t inst, loc_t loc) {
	pir_proc_t *procp = parser_current_proc();

	assert(parser_ctx.es_len < ARRAYLEN(parser_ctx.es));
	parser_ctx.es[parser_ctx.es_len++] = (parser_value_t){
		.kind = VAL_INST,
		.loc = loc,
		.type = procp->insts[inst].type,
		.d_inst = inst,
	};
}

void vpush_inst_back(pir_rinst_t inst) {
	pir_proc_t *procp = parser_current_proc();
	vpush_inst(inst, procp->insts[inst].loc);
}

void vinfix(pir_rblock_t bb, tok_t tok, loc_t loc) {
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

	if (tok != TOK_ASSIGN) {
		pir_rinst_t ilhs = vemit(bb, lhs);
		pir_rinst_t irhs = vemit(bb, rhs);
		inst = parser_inew(bb, (pir_inst_t){
			.loc = loc,
			.type = TYPE_UNRESOLVED,
			.kind = PIR_INFIX,
			.d_infix.op = tok,
			.d_infix.lhs = ilhs,
			.d_infix.rhs = irhs,
		});

		vpush_inst(inst, loc);
	} else {
		pir_rinst_t irhs = vemit(bb, rhs);
		// assign expressions do actually return the value
		// but must be reloaded for use in further expressions
		vstore(bb, lhs, irhs, loc);
		vpush_inst(irhs, loc);
	}
}

void vprefix(pir_rblock_t bb, tok_t tok, loc_t loc) {
	assert(parser_ctx.es_len >= 1);

	parser_value_t val = vpop();

	pir_rinst_t ival = vemit(bb, val);

	// TODO: resolve `TYPE_UNRESOLVED` if possible
	//       ....

	pir_rinst_t inst = parser_inew(bb, (pir_inst_t){
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
