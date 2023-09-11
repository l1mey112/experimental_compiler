#include "all.h"

#include "stb_ds.h"

static sym_t *table = NULL;

rsym_t table_new(sym_t sym) {
	if (sym.key >= 0) {
		assert(hmgeti(table, sym.key) == -1);
	}

	hmputs(table, sym);

	return (rsym_t)hmlenu(table) - 1;
}

// warning: this is an unstable pointer,
//          but that shouldn't matter after parsing.
sym_t *table_get(rsym_t rsym) {
	assert(rsym >= 0 && rsym < hmlenu(table)); // TODO: probably safe to remove? ASAN gets to this first
	return &table[rsym];
}

void table_dump(bool list_ir) {
	for (rsym_t i = 0; i < hmlenu(table); i++) {
		sym_t *sym = table_get(i);

		if (sym->key < 0) {
			continue;
		}

		const char *sym_kind_str = sym->kind == SYM_PROC ? "proc" : sym->kind == SYM_CONST ? "const" : "global";

		eprintf("%s%s: (%s)\n", sym->is_pub ? "pub " : "", sv_from(sym->key), sym_kind_str);

		switch (sym->kind) {
			case SYM_PROC:
				if (list_ir) {
					dump_proc(&sym->proc);
				}
				break;
			case SYM_CONST:
			case SYM_GLOBAL:
				assert_not_reached();
		}
	}
}
