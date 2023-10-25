#include "all.h"

#include "stb_ds.h"
#include <assert.h>

void check_type(type_t type, loc_t loc) {
	typeinfo_t *ts = type_get(type);
	if (ts->kind == TYPE_UNKNOWN) {
		assert(ts->is_named && "how did we get here?");
		err_with_pos(loc, "type `%s` already defined", fs_module_symbol_sv(ts->module, ts->name));
	}
}

// TODO: store the initial def of a variable??
//
// let v: T     | all variables with TYPE_UNKNOWN must have an initialiser
// let v = init |

type_t check_tlget(pir_proc_t *proc, pir_rlocal_t def) {
	pir_local_t *local = &proc->locals[def];
	assert(local->def != (pir_rinst_t)-1);
	assert(local->type != TYPE_UNKNOWN);
	return local->type;
}

type_t check_tget(pir_proc_t *proc, pir_rinst_t def) {
	pir_inst_t *ins = pir_at(proc, def);
	assert(ins->type != TYPE_UNKNOWN);
	return ins->type;
}

type_t check_sget(rsym_t sym) {
	assert(sym != SYM_UNRESOLVED);
	type_t type = table[sym].type;

	if (type == TYPE_UNKNOWN) {
		
	}
	
	return table[sym].type;
}

// infer type, by walking down the chain
type_t check_evaluate_type(fs_rnode_t module, pir_proc_t *proc, pir_rinst_t def) {
	/* pir_inst_t *ins = pir_at(proc, def);
	if (ins->type != TYPE_UNKNOWN) {
		return ins->type;
	}
	switch (ins->kind) {
		case PIR_INTEGER_LITERAL:
			return TYPE_I32;
		case PIR_LLOAD: {
			if (ins->d_load.is_sym) {
				table_resolve(&ins->d_load.sym, module, ins->loc);
				return;
			} else {
				return check_tlget(proc, ins->d_load.local);
			}
		}
		case PIR_NOP:
		default:
			assert_not_reached();
	} */
}

// infer type, by walking up the chain to a terminal instruction
// i.e: integer literals
type_t check_asertain_type(pir_proc_t *proc, pir_rinst_t def) {

}

void check_proc(sym_t *sym, pir_proc_t *proc) {
	// to check proc: (verify locals, verify body)
	//     verify type on local
	//     asertain type from local initialiser

	(void)sym;

	// all instructions reference previous instructions
	// just iterate
	for (u32 i = 0; i < arrlenu(proc->insts); i++) {
		check_evaluate_type(sym->module, proc, i);
	}

	/* for (u32 i = 0; i < arrlenu(proc->locals); i++) {
		pir_local_t *local = &proc->locals[i];
		if (local->type != TYPE_UNKNOWN) {
			// let v: T = ...
			// !TYPE_UNKNOWN -> type_loc is valid
			check_type(local->type, local->type_loc);
		} else {
			// let v = ...
			assert(local->def == (pir_rinst_t)-1);
			pir_inst_t *ins = pir_at(proc, local->def);
			assert(ins->kind == PIR_LSTORE);
			local->type = check_asertain_type(proc, ins->d_store.src);
		}
	} */
	
	// TODO: should type be asertained like normal??
	//       or should the proc be walked one by one?
	//
	//       i go for the second method.
	//       a feature of SSA is that all insts are
	//       defined before use, so we can walk the
	//       insts in order and check them as we go.

	// TODO: if current != local->def, and is assigned
	//       while being immut, error
}

void check_all(void) {
	/* for (rsym_t i = 0; i < hmlenu(table); i++) {
		sym_t *sym = &table[i];
		switch (sym->kind) {
			case SYM_PROC:
				check_proc(sym, &sym->proc);
				break;
			default:
				// TODO: handle constants, globals and such
				assert_not_reached();
		}
	} */
}