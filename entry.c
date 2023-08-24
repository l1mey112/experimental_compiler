#include <stdio.h>
#include <setjmp.h>

#include "hasc.h"
#include "ir.h"
#include "tok.h"
#include "parse.h"
#include "stb_ds.h"
#include "type.h"

const char* __asan_default_options() { return "detect_leaks=0"; }

int main(void) {
	hparser_t parser = {};
	hcc_ctx_t ctx = {};

	const char *work =
		"fn test(a: i32) {\n"
		"    b: i32\n"
		"    b = 23\n"
		"}";

	// -(10 + 2) + a

	printf("%s\n", work);
	hparser_init(&ctx, &parser, (u8 *)work, strlen(work));

	if (setjmp(ctx.err_buf) == 0) {
		hparser_run(&ctx, &parser);
		printf("passed!\n");
	} else {
		puts(ctx.err_msg);
		printf("nope!\n");
		return 1;
	}

	printf("------------ type_table(%lu): ------------\n", stbds_arrlenu(ctx.type_table));
	for (int i = 0; i < stbds_arrlen(ctx.type_table); i++) {
		printf("typeinfo(%d): ", i);
		htable_type_dump(&ctx, i + _HT_CONCRETE_MAX);
	}
	printf("------------ procs(%lu): ------------\n", stbds_arrlenu(ctx.procs));
	for (int i = 0; i < stbds_arrlen(ctx.procs); i++) {
		hproc_t *proc = &ctx.procs[i];
		hproc_dump(&ctx, proc);
	}
}