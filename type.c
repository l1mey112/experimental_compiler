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
	
	if (a->is_named && b->is_named) {
		// may be TYPE_UNKNOWN
		return a->module == b->module && a->name == b->name;
	}

	if (a_type != b_type) {
		return false;
	}

	switch (a_type) {
		case TYPE_PTR:
			return a->type_ref == b->type_ref;
		case TYPE_FN:
			if (a->d_fn.args_len != b->d_fn.args_len || a->d_fn.ret != b->d_fn.ret) {
				return false;
			}
			return memcmp(a->d_fn.args, b->d_fn.args, a->d_fn.args_len) == 0;
		default:
			assert_not_reached();
	}
}

// reference T -> TYPE_UNKNOWN 
// define T -> TYPE_!UNKNOWN probably TYPE_STRUCT
type_t type_new(typeinfo_t typeinfo, loc_t loc) {
	assert(typeinfo.kind >= _TYPE_CONCRETE_MAX);

	// intern types, there aren't that many in an average program
	// the same types are often used in the same context
	// so iterate backwards instead
	//
	// will also define types, but not redefine them
	for (u32 i = type_len; i > 0;) {
		i--;
		typeinfo_t *nb = &types[i];
		if (cmp_typeinfo(nb, &typeinfo)) {
			// for is_named:
			//     a->module == b->module && a->name == b->name
			// but may have different typeinfo_kind_t, meaning referencing same name
			if (typeinfo.is_named && nb->is_named && typeinfo.kind != nb->kind) {
				if (typeinfo.kind != TYPE_UNKNOWN) {
					// therefore nb->kind != TYPE_UNKNOWN, and is already defined
					err_with_pos(loc, "type '%s' already defined", fs_module_symbol_sv(typeinfo.module, typeinfo.name));
				}
			}

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

	// loc_t won't be accessed, .is_named = false
	return type_new(typeinfo, (loc_t){});
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
			assert(typeinfo->is_named);
			COMMIT(sprintf((char *)p, "%s", fs_module_symbol_sv(typeinfo->module, typeinfo->name)));
			return;
		case TYPE_PTR:
			*p = '*', p++;
			_type_dbg_str(typeinfo->type_ref);
			return;
		case TYPE_TUPLE:
			COMMIT(sprintf((char *)p, "("));
			for (u32 i = 0; i < typeinfo->d_tuple.len; i++) {
				type_t elem = typeinfo->d_tuple.elems[i];
				_type_dbg_str(elem);
				if (i + 1 < typeinfo->d_tuple.len) {
					COMMIT(sprintf((char *)p, ", "));
				}
			}
			COMMIT(sprintf((char *)p, ")"));
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
			if (typeinfo->d_fn.ret != TYPE_VOID) {
				COMMIT(sprintf((char *)p, ": "));
				_type_dbg_str(typeinfo->d_fn.ret);
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

	
	// ARGHHHHH
	p = alloc_scratch(1024);
	u8 *oldp = p;

	_type_dbg_str(type);

	u32 nwritten = p - oldp;

	p[nwritten] = '\0';

	return (const char *)oldp;
}