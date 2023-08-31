#pragma once

#include "tok.h"
#include "type.h"
#include "shared.h"

typedef struct hstmt_t hstmt_t;
typedef struct hproc_t hproc_t;
typedef struct hast_ident_t hast_ident_t;
typedef enum hast_kind_t hast_kind_t;
typedef u32 hblock_ref_t;

enum hstmt_kind_t {
	// one per argument
	arg,
	// variables
	// allocate (returns ptr), store and load from ptr
	// TODO: mem2reg?? hahh
	alloca,
	store,
	load,
	// integer and float, wrapping and overflow is UB (opt)
	// div has truncation for integers, and normal for float
	// rem is modulus division for float and integers
	//
	// TODO: add_safe,
	// TODO: add_wrap, 
	// TODO: add_sat, 
	// TODO: ... and so on
	// TODO: div_trunc
	// TODO: div_floor
	// TODO: ... and so on
	add,
	sub,
	mul,
	div,
	rem,
	// float and integer negation
	// reminder, 0 - float != -float (will effect sign of zero, inf, and NaN)
	// TODO: ffast-math ??
	neg,
	// TODO: ptr operations are all statically typed
	//       which means...
	// TODO: ptr_add,
	// TODO: ptr_sub,
	//
	// TODO: hmm, ptr arithmetic and indexing?
	//       should they be seperate ops?
	//       or operate on sizeof? (this would bloat IR code)
	//
	//       ideally there should be no notion of types in lowered IR
	//       should we lower this in here? or would it be a different IR?
	//       i mean, we aren't lowering retptrs, it's just return by value
	//
	// TODO: sizeof,
	// TODO: slice,
	//
	// TODO: selection over two operands, will be lowered in opt pass
	// TODO: select,
	//
	// bitstuffs
	bit_or,
	bit_and,
	bit_xor,
	bit_not,
	bit_shr,
	bit_shru,
	bit_shl,
	// comparisons
	// reminder: cmp_and + cmp_or are not short circuting
	cmp_lt,
	cmp_le,
	cmp_gt,
	cmp_ge,
	cmp_eq,
	cmp_neq,
	cmp_and,
	cmp_or,
	// TODO: should we even include intrinsics?
	//       these will be lowered down anyway... (wasm has sqrt and all)
	//       i say don't include them
	// TODO: bit_clz,
	// TODO: bit_ctz,
	// TODO: bit_popcnt,
	//
	// casting
	cast,
	// TODO: reinterpret_cast,
	//
	// TODO: slices?
	// TODO: slice,
	// TODO: slice_len,
	// TODO: slice_ptr,
	//
	// function calls
	// proper flags might not be known (inline, ...) until after checker
	// exprs like `inline call()` will have `force_inline` set
	// TODO: call_always_tail,
	//       from: return[tail] call()
	call,
	// branching
	// TODO: call with branch on `or {}`
	// TODO: unreachable
	//
	// TODO: more intrins to be lowered from:
	// TODO: memcpy,
	// TODO: memset,
	// TODO: wasm_memory_size,
	// TODO: wasm_memory_grow,
}

// TODO: debuginfo inline is easiest, most places will have it filled anyways

// three address code - single assignment form
// similar to zig AIR
struct hstmt_t {
	hstmt_kind_t kind;
	/* htype_t type;
	union {
		struct {

		} d_arg;
	}; */
};

enum hterminator_kind_t {
	branch,
	branch_if,
	// TODO: switch ?
	unwrap_call,
};

struct hterminator_t {
	hterminator_kind_t kind;
	hblock_ref_t targets[2];
};

struct hblock_t {
	hstmt_t kind;
	hterminator_t terminator;
};

// TODO: index to a debuginfo token?

enum hast_kind_t {
	HAST_STMT_EXPR, // TODO: retire STMT EXPR, it doesn't make sense...
	HAST_STMT_RETURN,
	HAST_EXPR_LITERAL_INT,
	HAST_EXPR_LITERAL_FLOAT,
	HAST_EXPR_IDENT,
	HAST_EXPR_PREFIX,
	HAST_EXPR_INFIX,
};

// `next` for linked list of stuffs
// comma EXPR
struct hast_node_t {
	hast_kind_t kind;
	htoken_t token;
	u32 children[3];
	u32 next; // linked list
	union {
		struct {
			u32 idx;
			bool is_local;
		} d_ident;
		struct {
			u64 value;
		} d_literal_int;
		struct {
			f64 value;
		} d_literal_float;
	};
};

struct hast_ident_t {
	htoken_t token;
	size_t name_hash;
	htype_t type;
	bool is_arg;
};

struct hproc_t {
	u32 ast_begin;
	htoken_t fn_name;
	hast_ident_t *locals;
	htype_t fn_type;
	bool is_extern;
};

void hproc_dump(hcc_ctx_t *ctx, hproc_t *proc);