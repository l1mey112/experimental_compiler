#include "all.h"

#include "stb_ds.h"

sym_t *table = NULL;

rsym_t table_new(loc_t loc, sym_t sym) {
	if (hmgeti(table, sym.key) != -1) {
		err_with_pos(loc, "error: symbol `%s` already defined", sv_from(sym.key));
	}

	hmputs(table, sym);

	return (rsym_t)hmlenu(table) - 1;
}

// returns SYM_UNRESOLVED if not found
rsym_t table_retrieve_field(fs_rnode_t mod, istr_t lit) {
	// build up a path to the field in memory
	const char *sv = fs_module_symbol_sv(mod, lit);

	// TODO: im lazy! whatever...
	//       there is no such spurrious interrupt to corrupt scratch
	alloc_reset((u8*)sv);

	ptrdiff_t idx;
	if ((idx = sv_index(sv)) == -1) {
		return SYM_UNRESOLVED;
	}

	ptrdiff_t tidx;
	if ((tidx = hmgeti(table, (istr_t)idx)) == -1) {
		return SYM_UNRESOLVED;
	}

	return (rsym_t)tidx;
}

// attempt to resolve
void table_resolve(sym_resolve_t *resolve, fs_rnode_t src_module, loc_t loc) {
	if (resolve->sym != SYM_UNRESOLVED) {
		return;
	}
	rsym_t sym;
	if ((sym = table_retrieve_field(resolve->d_unresolved.module, resolve->d_unresolved.lit)) == SYM_UNRESOLVED) {
		err_with_pos(loc, "error: unresolved symbol `%s`", sv_from(resolve->d_unresolved.lit));
	}
	sym_t *symp = &table[sym];
	if (symp->module != src_module && !symp->is_pub) {
		err_with_pos(loc, "error: symbol `%s` is not public in module `%s`", sv_from(resolve->d_unresolved.lit), fs_module_symbol_sv(src_module, (istr_t)-1));
	}

	resolve->sym = sym;
}

void table_dump(bool list_ir) {
	for (rsym_t i = 0; i < hmlenu(table); i++) {
		sym_t *sym = &table[i];

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
			case SYM_CONST: {
				eprintf("\t:%u -> :%u\n", sym->constant.bb_start, sym->constant.bb_end);
				break;
			}
			case SYM_GLOBAL:
				assert_not_reached();
		}
	}
}
