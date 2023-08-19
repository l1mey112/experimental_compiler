#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>

#include "hasc.h"
#include "lex.h"

// hcc_err(ctx, "<src>:%u:%u: unexpected character '%c'", row + 1, col + 1, ch);

void hcc_err_with_pos(hcc_ctx_t *ctx, hlex_token_t tok, const char *fmt, ...) {
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

int main(void) {
	hcc_ctx_t ctx = {};

	const char *work = "extern fn";

	hparser_init(&ctx, (u8 *)work, strlen(work));

	if (setjmp(ctx.err_buf) == 0) {
		hparser_run(&ctx);
		printf("passed!\n");
	} else {
		puts(ctx.err_msg);
		printf("nope!\n");
	}
}