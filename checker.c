#include "all.h"

#include "stb_ds.h"

void check_proc(sym_t *sym) {
	(void)sym;
}

void check_all(void) {
	for (rsym_t i = 0; i < hmlenu(table); i++) {
		sym_t *sym = &table[i];
		switch (sym->kind) {
		case SYM_PROC:
			check_proc(sym);
			break;
		default:
			// TODO: handle constants, globals and such
			assert_not_reached();
		}
	}
}