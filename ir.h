#pragma once

#include "lex.h"
#include "type.h"
#include "shared.h"

typedef struct hast_node_t hast_node_t;
typedef struct hcfg_node_t hcfg_node_t;
typedef struct hproc_t hproc_t;
typedef struct harg_t harg_t;
typedef struct hast_ident_t hast_ident_t;
typedef enum hast_type_t hast_type_t;

enum hast_type_t {
	HAST_STMT_EXPR, // TODO: retire STMT EXPR, it doesn't make sense...
	HAST_EXPR_LITERAL_INT,
	HAST_EXPR_LITERAL_FLOAT,
	HAST_EXPR_IDENT,
	HAST_EXPR_PREFIX,
	HAST_EXPR_INFIX,
};

struct hcfg_node_t {
	u32 nidx;
	u32 ast_begin;
	u32 ast_cond;  // if -1, goto true, else false
	u32 node_true;
	u32 node_false;
};

// TODO: why have `children[3]` when `next` is available?
struct hast_node_t {
	hast_type_t type;
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
	u32 cfg_begin; // -1, for no body
	htoken_t fn_name;
	hast_ident_t *locals;
	htype_t fn_type;
	bool is_extern;
};

void hproc_dump(hcc_ctx_t *ctx, hproc_t *proc);