#include "hasc.h"
#include "shared.h"
#include "type.h"
#include "stb_ds.h"
#include <stdio.h>
#include <string.h>

static bool htable_compare_typeinfo(htypeinfo_t *a, htypeinfo_t *b) {
	htypeinfo_kind_t a_type = a->type, b_type = b->type;

	if (a_type != b_type) {
		return false;
	}

	switch (a_type) {
		case HT_UNKNOWN:
			return a->d_unknown.name_hash == b->d_unknown.name_hash;
		case HT_PTR:
		case HT_OPTION:
			return a->type_ref == b->type_ref;
		case HT_FN:
			if (a->d_fn.args_len != b->d_fn.args_len || a->d_fn.rets_len != b->d_fn.rets_len) {
				return false;
			}
			return memcmp(a->d_fn.args, b->d_fn.args, a->d_fn.args_len) == 0 && memcmp(a->d_fn.rets, b->d_fn.rets, a->d_fn.rets_len) == 0;
		default:
			assert_not_reached();
	}
	
	return false;
}

htype_t htable_intern_append(hcc_ctx_t *ctx, htypeinfo_t type) {
	assert(type.type >= _HT_CONCRETE_MAX);

	size_t size = stbds_arrlen(ctx->type_table);

	// intern types
	for (size_t i = 0; i < size; i++) {
		if (htable_compare_typeinfo(&ctx->type_table[i], &type)) {
			return i + _HT_CONCRETE_MAX;
		}
	}

	// mallocate
	if (type.type == HT_FN) {
		if (type.d_fn.args_len == 0) {
			type.d_fn.args = NULL;
		} else {
			type.d_fn.args = (htype_t*)memdup((u8*)type.d_fn.args,  type.d_fn.args_len * sizeof(htype_t));
		}
		if (type.d_fn.rets_len == 0) {
			type.d_fn.rets = NULL;
		} else {
			type.d_fn.rets = (htype_t*)memdup((u8*)type.d_fn.rets,  type.d_fn.rets_len * sizeof(htype_t));
		}
	}

	htype_t idx = size + _HT_CONCRETE_MAX;
	stbds_arrpush(ctx->type_table, type);
	return idx;
}

htype_t htable_type_get(hcc_ctx_t *ctx, htoken_t token) {
	htype_t type;

	if (0);
	else if (hsv_memcmp_literal(token.p, token.len, "i8")) type = HT_I8;
	else if (hsv_memcmp_literal(token.p, token.len, "i16")) type = HT_I16;
	else if (hsv_memcmp_literal(token.p, token.len, "i32")) type = HT_I32;
	else if (hsv_memcmp_literal(token.p, token.len, "i64")) type = HT_I64;
	else if (hsv_memcmp_literal(token.p, token.len, "isize")) type = HT_ISIZE;
	else if (hsv_memcmp_literal(token.p, token.len, "u8")) type = HT_U8;
	else if (hsv_memcmp_literal(token.p, token.len, "u16")) type = HT_U16;
	else if (hsv_memcmp_literal(token.p, token.len, "u32")) type = HT_U32;
	else if (hsv_memcmp_literal(token.p, token.len, "u64")) type = HT_U64;
	else if (hsv_memcmp_literal(token.p, token.len, "usize")) type = HT_USIZE;
	else if (hsv_memcmp_literal(token.p, token.len, "f32")) type = HT_F32;
	else if (hsv_memcmp_literal(token.p, token.len, "f64")) type = HT_F64;
	else if (hsv_memcmp_literal(token.p, token.len, "bool")) type = HT_BOOL;	
	else {
		// unknown type, create a new one
		htypeinfo_t typeinfo = {
			.type = HT_UNKNOWN,
			.d_unknown.name_hash = stbds_hash_bytes(token.p, token.len, 99999),
			.d_unknown.token = token,
		};
		type = htable_intern_append(ctx, typeinfo);
	}

	return type;
}

htype_t htable_type_inc_muls(hcc_ctx_t *ctx, htype_t type) {
	htypeinfo_t typeinfo = {
		.type = HT_PTR,
		.type_ref = type,
	};

	return htable_intern_append(ctx, typeinfo);
}

htype_t htable_type_option_of(hcc_ctx_t *ctx, htype_t type) {
	htypeinfo_t typeinfo = {
		.type = HT_OPTION,
		.type_ref = type,
	};

	return htable_intern_append(ctx, typeinfo);
}

const char *htypeinfo_kind_concrete_str[_HT_CONCRETE_MAX] = {
	[HT_VOID] = "void",
	[HT_I8] = "i8",
	[HT_I16] = "i16",
	[HT_I32] = "i32",
	[HT_I64] = "i64",
	[HT_ISIZE] = "isize",
	[HT_U8] = "u8",
	[HT_U16] = "u16",
	[HT_U32] = "u32",
	[HT_U64] = "u64",
	[HT_USIZE] = "usize",
	[HT_F32] = "f32",
	[HT_F64] = "f64",
	[HT_BOOL] = "bool",
};

void _htable_type_dump(hcc_ctx_t *ctx, htype_t type) {
	while (1) {
		if (type < _HT_CONCRETE_MAX) {
			printf("%s", htypeinfo_kind_concrete_str[type]);
			return;
		}

		htypeinfo_t *typeinfo = htable_typeinfo_get(ctx, type);

		switch (typeinfo->type) {
			case HT_PTR:
				printf("*");
				type = typeinfo->type_ref;
				break;
			case HT_OPTION:
				printf("?");
				type = typeinfo->type_ref;
				break;
			case HT_FN:
				printf("fn (");
				for (u32 i = 0; i < typeinfo->d_fn.args_len; i++) {
					htype_t arg = typeinfo->d_fn.args[i];
					_htable_type_dump(ctx, arg);
					if (i + 1 < typeinfo->d_fn.args_len) {
						printf(", ");
					}
				}
				printf(")");
				if (typeinfo->d_fn.rets_len == 1) {
					printf(": ");
					htype_t ret = typeinfo->d_fn.rets[0];
					_htable_type_dump(ctx, ret);
				} else if (typeinfo->d_fn.rets_len > 1) {
					printf(": (");
					for (u32 i = 0; i < typeinfo->d_fn.rets_len; i++) {
						htype_t ret = typeinfo->d_fn.rets[i];
						_htable_type_dump(ctx, ret);
						if (i + 1 < typeinfo->d_fn.rets_len) {
							printf(", ");
						}
					}
					printf(")");
				}
				return;
			case HT_UNKNOWN:
				printf("%.*s", (int)typeinfo->d_unknown.token.len, typeinfo->d_unknown.token.p);
				return;
			default:
				assert_not_reached();
		}
	}
}

void htable_type_dump(hcc_ctx_t *ctx, htype_t type) {
	_htable_type_dump(ctx, type);
	printf("\n");
}

htypeinfo_t *htable_typeinfo_get(hcc_ctx_t *ctx, htype_t type) {
	assert(type >= _HT_CONCRETE_MAX);

	size_t idx = type - _HT_CONCRETE_MAX;
	assert(idx < stbds_arrlenu(ctx->type_table));
	
	return &ctx->type_table[idx];
}