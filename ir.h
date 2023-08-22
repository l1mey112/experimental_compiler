#pragma once

#include "lex.h"

typedef struct htype_t htype_t;

// 32 bit
struct htype_t {
	u16 tidx;
	u8 nr_muls;
};

typedef struct hast_node_t hast_node_t;
typedef struct hcfg_node_t hcfg_node_t;
typedef struct hproc_t hproc_t;
typedef struct harg_t harg_t;

struct hcfg_node_t {
	u32 ast_begin;
};

struct hast_node_t {
	htok_t kind;
	u64 value;
	u32 children[2];
};

struct harg_t {
	owned_string name;
	htype_t type;
};

struct hproc_t {
	harg_t *args;
	harg_t *rets;
	u32 cfg_begin;
	bool is_extern;
};