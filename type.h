#pragma once

#include "lex.h"

typedef struct htype_t htype_t;
typedef enum htype_kind_t htype_kind_t;

// 32 bit
struct htype_t {
	htype_kind_t tidx;
	u8 nr_muls;

	// ?&&T is represented as a single type
	// can't have a ?&?&T

	/* union {
		struct {
			bool atomic : 1;
			bool nullable : 1;
		};
		u8 flags;
	}; */
};

enum htype_kind_t {
	HT_UNKNOWN,
	HT_VOID,
	HT_I8,
	HT_I16,
	HT_I32,
	HT_I64,
	HT_U8,
	HT_U16,
	HT_U32,
	HT_U64,
	HT_USIZE,
	HT_ISIZE,
	HT_BOOL,
	HT_F32,
	HT_F64,
	// HT_FN_PTR,
	// HT_STRUCT,
	// HT_ARRAY,
	// HT_FIXEDARRAY,
	// HT_ENUM,
}
