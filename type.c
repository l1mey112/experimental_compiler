#include "all.h"
#include <assert.h>

static typeinfo_t table[1024];
static u32 table_len;

static const char *ctinfo_str[_TYPE_CONCRETE_MAX] = {
	#define X(name, lit) [name] = lit,
    TYPE_X_CONCRETE_LIST
    #undef X
};

static bool cmp_typeinfo(typeinfo_t *a, typeinfo_t *b) {
	typeinfo_kind_t a_type = a->kind, b_type = b->kind;

	if (a_type != b_type) {
		return false;
	}

	switch (a_type) {
		case TYPE_UNKNOWN:
			return a->d_unknown.lit == b->d_unknown.lit;
		case TYPE_PTR:
			return a->type_ref == b->type_ref;
		case TYPE_FN:
			if (a->d_fn.args_len != b->d_fn.args_len || a->d_fn.rets_len != b->d_fn.rets_len) {
				return false;
			}
			return memcmp(a->d_fn.args, b->d_fn.args, a->d_fn.args_len) == 0 && memcmp(a->d_fn.rets, b->d_fn.rets, a->d_fn.rets_len) == 0;
		default:
		assert_not_reached();
	}
}

type_t table_new(typeinfo_t typeinfo) {
	assert(typeinfo.kind >= _TYPE_CONCRETE_MAX);

	// intern types, there aren't that many in an average program
	// the same types are often used in the same context
	// so iterate backwards instead
	for (u32 i = table_len; i > 0;) {
		i--;
		if (cmp_typeinfo(&table[i], &typeinfo)) {
			return i + _TYPE_CONCRETE_MAX;
		}
	}

	// assert(size <= _TYPE_CONCRETE_MAX || size - _TYPE_CONCRETE_MAX < UINT16_MAX);
	assert(table_len < ARRAYLEN(table));

	type_t type = table_len + _TYPE_CONCRETE_MAX;
	table[table_len++] = typeinfo;
	return type;
}

typeinfo_t *table_get(type_t type) {
	assert(type >= _TYPE_CONCRETE_MAX);
	u32 idx = type - _TYPE_CONCRETE_MAX;
	assert(idx < table_len);
	return &table[idx];
}

type_t table_new_inc_mul(type_t type) {
	typeinfo_t typeinfo = {
		.kind = TYPE_PTR,
		.type_ref = type,
	};

	return table_new(typeinfo);
}

static u32 _table_type_dbg_str(u8 *p, type_t type) {
	// good? bad? im the one with the sloppy macros
	#define CPY_LIT_FR(p, lit) (memcpy(p, lit, sizeof(lit "") - 1), p += sizeof(lit "") - 1, sizeof(lit "") - 1)
	#define CPY_LIT(lit) CPY(CPY_LIT_FR(p, lit))
	#define CPY_CH(ch) (p[0] = ch, CPY(1))
	#define CPY(v) ({ \
		u32 nv = (v); \
		p += nv; \
		nwritten += nv; \
	})

	if (type < _TYPE_CONCRETE_MAX) {
		return sprintf((char *)p, "%s", ctinfo_str[type]);
	}

	typeinfo_t *typeinfo = table_get(type);

	if (typeinfo->kind == TYPE_UNKNOWN) {
		return sprintf((char *)p, "%s", sv_from(typeinfo->d_unknown.lit));
	}
		
	u32 nwritten = 0;

	switch (typeinfo->kind) {
		case TYPE_PTR:
			CPY_CH('*');
			CPY(_table_type_dbg_str(p, typeinfo->type_ref));
			return nwritten;
		case TYPE_FN:
			CPY_LIT("fn (");
			for (u32 i = 0; i < typeinfo->d_fn.args_len; i++) {
				type_t arg = typeinfo->d_fn.args[i];
				CPY(_table_type_dbg_str(p, arg));
				if (i + 1 < typeinfo->d_fn.args_len) {
					CPY_LIT(", ");
				}
			}
			CPY_CH(')');
			if (typeinfo->d_fn.rets_len == 1) {
				CPY_LIT(": ");
				CPY(_table_type_dbg_str(p, typeinfo->d_fn.rets[0]));
			} else if (typeinfo->d_fn.rets_len > 1) {
				CPY_LIT(": (");
				for (u32 i = 0; i < typeinfo->d_fn.rets_len; i++) {
					type_t ret = typeinfo->d_fn.rets[i];
					CPY(_table_type_dbg_str(p, ret));
					if (i + 1 < typeinfo->d_fn.rets_len) {
						CPY_LIT(", ");
					}
				}
				CPY_CH(')');
			}
			return nwritten;
		default:
			assert_not_reached();
	}

	#undef CPY_LIT_FR
	#undef CPY_LIT
	#undef CPY_CH
	#undef CPY
}

// allocates using alloc_* functions
const char *table_type_dbg_str(type_t type) {
	u8 *p = alloc_scratch(0);
	u32 len = _table_type_dbg_str(p, type);
	p[len] = '\0'; // nul not guarenteed, place anyway

	return (const char *)alloc_scratch(len + 1);
}