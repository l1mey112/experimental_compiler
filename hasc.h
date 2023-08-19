#pragma once

#include "shared.h"
#include "parse.h"
#include "lex.h"
#include "ir.h"

#include <setjmp.h>

typedef struct hcc_ctx_t hcc_ctx_t;

struct hcc_ctx_t {
	hparser_t parser;
	jmp_buf err_buf;
	char err_msg[256];
	struct {
		hcfg_node_t *node_arena;
		hast_node_t *ast_arena;
		hproc_t *proc_arena;
	} arena;
};

noreturn void hcc_err(hcc_ctx_t *ctx, const char *fmt, ...);
noreturn void hcc_err_with_pos(hcc_ctx_t *ctx, hlex_token_t tok, const char *fmt, ...);