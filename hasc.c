#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#undef STB_DS_IMPLEMENTATION

#include "hasc.h"
#include "lex.h"

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
