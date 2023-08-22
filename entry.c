#include <stdio.h>
#include <setjmp.h>

#include "hasc.h"
#include "lex.h"
#include "stb_ds.h"
#include "type.h"

int main(void) {
	hcc_ctx_t ctx = {};

	const char *work = "extern fn test(a: i32, b: T): ?T {}";

	printf("%s\n", work);
	hparser_init(&ctx, (u8 *)work, strlen(work));

	if (setjmp(ctx.err_buf) == 0) {
		hparser_run(&ctx);
		printf("passed!\n");
	} else {
		puts(ctx.err_msg);
		printf("nope!\n");
	}

	printf("------------ type_table(%lu): ------------\n", stbds_arrlenu(ctx.type_table));
	for (int i = 0; i < stbds_arrlen(ctx.type_table); i++) {
		printf("typeinfo(%d): ", i);
		htable_type_dump(&ctx, i + _HT_CONCRETE_MAX);
	}
}