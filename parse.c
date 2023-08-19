#include <stdio.h>

#include "parse.h"
#include "hasc.h"
#include "lex.h"

void hparser_init(hcc_ctx_t *ctx, u8 *pc, size_t plen) {
	hlex_init(&ctx->parser.lex_c, pc, plen);
}

static void hparser_next(hcc_ctx_t *ctx) {
	ctx->parser.tok = hlex_next(&ctx->parser.lex_c, ctx);
}

static void hparser_top_stmt(hcc_ctx_t *ctx) {
	hlex_token_t token = ctx->parser.tok;

	printf("yes %s!\n", htok_name(token.type));
	
	switch (token.type) {
		case htok_fn:
			printf("fn\n");
			break;
		case htok_extern:
			printf("extern\n");
			break;
		default:
			hcc_err_with_pos(ctx, token, "expected function or extern");
			break;
	}
}

void hparser_run(hcc_ctx_t *ctx) {


	while(!hlex_is_eof(&ctx->parser.lex_c)) {
		hparser_next(ctx);
		hparser_top_stmt(ctx);
	}
}