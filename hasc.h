#pragma once

#include "shared.h"
#include "parse.h"
#include "ir.h"
#include "type.h"

#include <setjmp.h>

typedef struct hcc_ctx_t hcc_ctx_t;

struct hcc_ctx_t {
	jmp_buf err_buf;
	char err_msg[256];

	struct {
		hcfg_node_t *cfg_arena;
		hast_node_t *ast_arena;
	} arena;
	hproc_t *current_proc; // should be moved into parser? maybe not?
	hproc_t *procs;
	htypeinfo_t *type_table;
	//

};
// current_proc
//   this may be a use after free!
//   or maybe not, as `current_proc` is only valid for the length of the parsing

noreturn void hcc_err(hcc_ctx_t *ctx, const char *fmt, ...);
noreturn void hcc_err_with_pos(hcc_ctx_t *ctx, htoken_t tok, const char *fmt, ...)
	__attribute__((format(printf, 3, 4)));

hcfg_node_t *hcc_cfg_node(hcc_ctx_t *ctx, u32 node);
hast_node_t *hcc_ast_node(hcc_ctx_t *ctx, u32 node);
hcfg_node_t *hcc_cfg_node_opt(hcc_ctx_t *ctx, u32 node);
hast_node_t *hcc_ast_node_opt(hcc_ctx_t *ctx, u32 node);

#define FOR_PIN_AST(node, name) \
	for (hast_node_t *name = hcc_ast_node(ctx, (node));true;)

#define FOR_PIN_CFG(node, name) \
	for (hcfg_node_t *name = hcc_cfg_node(ctx, (node));true;)

