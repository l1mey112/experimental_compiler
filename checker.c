#include "all.h"

#include "stb_ds.h"

void check_type(type_t type, loc_t loc) {
	typeinfo_t *ts = type_get(type);
	if (ts->kind == TYPE_UNKNOWN) {
		assert(ts->is_named && "how did we get here?");
		err_with_pos(loc, "type '%s' already defined", fs_module_symbol_sv(ts->module, ts->name));
	}
}

void check_proc(sym_t *sym, pir_proc_t *proc) {
	
}

void check_all(void) {
	for (rsym_t i = 0; i < hmlenu(table); i++) {
		sym_t *sym = &table[i];
		switch (sym->kind) {
			case SYM_PROC:
				check_proc(sym, &sym->proc);
				break;
			default:
				// TODO: handle constants, globals and such
				assert_not_reached();
		}
	}
}