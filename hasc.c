#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#undef STB_DS_IMPLEMENTATION

#include "hasc.h"
#include "tok.h"

void hcc_err_with_pos(hcc_ctx_t *ctx, htoken_t tok, const char *fmt, ...) {
	char buf[128];

	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	snprintf(ctx->err_msg, sizeof(ctx->err_msg), "<src>:%u:%u: %s", tok.row + 1, tok.col + 1, buf);

	longjmp(ctx->err_buf, 1);
}

void hcc_err(hcc_ctx_t *ctx, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vsnprintf(ctx->err_msg, sizeof(ctx->err_msg), fmt, args);
	va_end(args);
	longjmp(ctx->err_buf, 1);
}

hcfg_node_t *hcc_cfg_node(hcc_ctx_t *ctx, u32 node) {
	assert(node != (u32)-1);
	assert(node < stbds_arrlenu(ctx->arena.cfg_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.cfg_arena[node];
}

hast_node_t *hcc_ast_node(hcc_ctx_t *ctx, u32 node) {
	assert(node != (u32)-1);
	assert(node < stbds_arrlenu(ctx->arena.ast_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.ast_arena[node];
}

hcfg_node_t *hcc_cfg_node_opt(hcc_ctx_t *ctx, u32 node) {
	if (node == (u32)-1) {
		return NULL;
	}
	assert(node < stbds_arrlenu(ctx->arena.cfg_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.cfg_arena[node];
}

hast_node_t *hcc_ast_node_opt(hcc_ctx_t *ctx, u32 node) {
	if (node == (u32)-1) {
		return NULL;
	}
	assert(node < stbds_arrlenu(ctx->arena.ast_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.ast_arena[node];
}