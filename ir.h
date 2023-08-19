#pragma once

#include "lex.h"

typedef struct hast_node_t hast_node_t;
typedef struct hcfg_node_t hcfg_node_t;
typedef struct hproc_t hproc_t;

struct hcfg_node_t {
	u32 ast_begin;
};

struct hast_node_t {
	htok_t kind;
	u64 value;
	u32 children[2];
};

struct hproc_t {
	u32 cfg_begin;
};