#pragma once

#include "shared.h"
#include "parse.h"
#include "lex.h"
#include "ir.h"
#include "type.h"

#include <setjmp.h>

typedef struct hcc_ctx_t hcc_ctx_t;

struct hcc_ctx_t {
	hparser_t parser; // TODO: retire hparser_t, maybe inline it in a struct here?
	jmp_buf err_buf;
	char err_msg[256];

	// struct {
		// hcfg_node_t *node_arena;
		// hast_node_t *ast_arena;
	// } arena;
	hproc_t *current_proc;
	hproc_t *procs;
	htypeinfo_t *type_table;
	//

};

noreturn void hcc_err(hcc_ctx_t *ctx, const char *fmt, ...);
noreturn void hcc_err_with_pos(hcc_ctx_t *ctx, htoken_t tok, const char *fmt, ...);