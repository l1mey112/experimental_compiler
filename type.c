#include "all.h"
#include <assert.h>

static typeinfo_t types[1024];
static u32 type_len;

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

type_t type_new(typeinfo_t typeinfo) {
	assert(typeinfo.kind >= _TYPE_CONCRETE_MAX);

	// intern types, there aren't that many in an average program
	// the same types are often used in the same context
	// so iterate backwards instead
	for (u32 i = type_len; i > 0;) {
		i--;
		if (cmp_typeinfo(&types[i], &typeinfo)) {
			return i + _TYPE_CONCRETE_MAX;
		}
	}

	// assert(size <= _TYPE_CONCRETE_MAX || size - _TYPE_CONCRETE_MAX < UINT16_MAX);
	assert(type_len < ARRAYLEN(types));

	type_t type = type_len + _TYPE_CONCRETE_MAX;
	types[type_len++] = typeinfo;
	return type;
}

typeinfo_t *type_get(type_t type) {
	assert(type >= _TYPE_CONCRETE_MAX);
	u32 idx = type - _TYPE_CONCRETE_MAX;
	assert(idx < type_len);
	return &types[idx];
}

type_t type_new_inc_mul(type_t type) {
	typeinfo_t typeinfo = {
		.kind = TYPE_PTR,
		.type_ref = type,
	};

	return type_new(typeinfo);
}

static u8 *p;

static void _type_dbg_str(type_t type) {
	#define COMMIT(expr) \
		do { \
			p += (expr); \
		} while (0)
		

	if (type < _TYPE_CONCRETE_MAX) {
		COMMIT(sprintf((char *)p, "%s", ctinfo_str[type]));			
		return;
	}

	typeinfo_t *typeinfo = type_get(type);

	switch (typeinfo->kind) {
		case TYPE_UNKNOWN:
			COMMIT(sprintf((char *)p, "%s", sv_from(typeinfo->d_unknown.lit)));
			return;
		case TYPE_PTR:
			*p = '*', p++;
			_type_dbg_str(typeinfo->type_ref);
			return;
		case TYPE_FN:
			COMMIT(sprintf((char *)p, "fn ("));
			for (u32 i = 0; i < typeinfo->d_fn.args_len; i++) {
				type_t arg = typeinfo->d_fn.args[i];
				_type_dbg_str(arg);
				if (i + 1 < typeinfo->d_fn.args_len) {
					COMMIT(sprintf((char *)p, ", "));
				}
			}
			COMMIT(sprintf((char *)p, ")"));
			if (typeinfo->d_fn.rets_len == 1) {
				COMMIT(sprintf((char *)p, ": "));
				_type_dbg_str(typeinfo->d_fn.rets[0]);
			} else if (typeinfo->d_fn.rets_len > 1) {
				COMMIT(sprintf((char *)p, ": ("));
				for (u32 i = 0; i < typeinfo->d_fn.rets_len; i++) {
					type_t ret = typeinfo->d_fn.rets[i];
					_type_dbg_str(ret);
					if (i + 1 < typeinfo->d_fn.rets_len) {
						COMMIT(sprintf((char *)p, ", "));
					}
				}
				COMMIT(sprintf((char *)p, ")"));
			}
			return;
		default:
			assert_not_reached();
	}
}

// allocates using alloc_* functions
const char *type_dbg_str(type_t type) {
	// u8 *p = alloc_scratch(0);
	//u32 len = _type_dbg_str(p, type);

	
	p = alloc_scratch(0);
	u8 *oldp = p;

	_type_dbg_str(type);

	u32 nwritten = p - oldp;

	p[nwritten] = '\0';

	return (const char *)alloc_scratch(nwritten + 1);
}