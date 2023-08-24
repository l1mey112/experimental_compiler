#pragma once

#include "tok.h"
#include "shared.h"

typedef u16 htype_t;
typedef enum htypeinfo_kind_t htypeinfo_kind_t;
typedef struct htypeinfo_t htypeinfo_t;

typedef struct hcc_ctx_t hcc_ctx_t;

enum htypeinfo_kind_t {
	HT_VOID,
	HT_I8,
	HT_I16,
	HT_I32,
	HT_I64,
	HT_ISIZE,
	HT_U8,
	HT_U16,
	HT_U32,
	HT_U64,
	HT_USIZE,
	HT_F32,
	HT_F64,
	HT_BOOL,
	_HT_CONCRETE_MAX,
	//
	HT_FN,
	HT_PTR,
	HT_OPTION,
	HT_ARRAY,
	HT_ENUM,
	HT_UNKNOWN,
	// HT_FN_PTR,
	// HT_STRUCT,
	// HT_FIXEDARRAY,
};

extern const char *htypeinfo_kind_concrete_str[_HT_CONCRETE_MAX];

struct htypeinfo_t {
	htypeinfo_kind_t type;

	union {
		struct {
			htoken_t token;
			size_t name_hash;
		} d_unknown;
		struct {
			htype_t *args;
			u32 args_len;
			htype_t *rets;
			u32 rets_len;
			// flags?
		} d_fn;
		htype_t type_ref;
	};

	/* union {
		struct {
			bool atomic : 1;
			bool nullable : 1;
		};
		u8 flags;
	}; */
};

static inline bool htype_is_signed(htype_t type) {
	return type >= HT_I8 && type <= HT_ISIZE;
}

static inline bool htype_is_unsigned(htype_t type) {
	return type >= HT_U8 && type <= HT_USIZE;
}

htype_t htable_intern_append(hcc_ctx_t *ctx, htypeinfo_t type);
htype_t htable_type_get(hcc_ctx_t *ctx, htoken_t token);
htype_t htable_type_inc_muls(hcc_ctx_t *ctx, htype_t type);
htype_t htable_type_option_of(hcc_ctx_t *ctx, htype_t type);
htypeinfo_t *htable_typeinfo_get(hcc_ctx_t *ctx, htype_t type);
void htable_type_dump(hcc_ctx_t *ctx, htype_t type);