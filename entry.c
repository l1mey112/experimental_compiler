#include <stdio.h>
#include <setjmp.h>

#include "hasc.h"
#include "lex.h"

int main(void) {
	hcc_ctx_t ctx = {};

	const char *work = "extern fn test(a: i32, b: i32): i32 {}";

	hparser_init(&ctx, (u8 *)work, strlen(work));

	if (setjmp(ctx.err_buf) == 0) {
		hparser_run(&ctx);
		printf("passed!\n");
	} else {
		puts(ctx.err_msg);
		printf("nope!\n");
	}
}