#pragma once

#include "lex.h"
#include "type.h"

typedef struct hast_node_t hast_node_t;
typedef struct hcfg_node_t hcfg_node_t;
typedef struct hproc_t hproc_t;
typedef struct harg_t harg_t;
typedef struct hast_ident_t hast_ident_t;
typedef enum hast_ast_type_t hast_ast_type_t;

enum hast_ast_type_t {
	HAST_LITERAL,
	HAST_IDENT,
	HAST_BINOP,
};

struct hcfg_node_t {
	u32 ast_begin;
};

struct hast_node_t {
	htok_t kind;
	u64 value;
	u32 children[2];
};

struct hast_ident_t {
	htoken_t token;
	size_t name_hash;
	htype_t type;
	bool is_arg;
};

struct hproc_t {
	u32 cfg_begin;
	htype_t fn_type;
	hast_ident_t *locals;
	bool is_extern;
};
