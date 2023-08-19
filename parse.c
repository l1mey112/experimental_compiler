#include <stdio.h>

#include "ir.h"
#include "parse.h"
#include "hasc.h"
#include "lex.h"

void hparser_init(hcc_ctx_t *ctx, u8 *pc, size_t plen) {
	hlex_init(&ctx->parser.lex_c, pc, plen);
}

static void hparser_next(hcc_ctx_t *ctx) {
	ctx->parser.tok = hlex_next(&ctx->parser.lex_c, ctx);
	hlex_token_dump(ctx->parser.tok);
}

static bool hparser_next_if_not_eof(hcc_ctx_t *ctx) {
	if (hlex_is_eof(&ctx->parser.lex_c)) {
		return false;
	}
	hparser_next(ctx);
	return true;
}

static void hparser_expect(hcc_ctx_t *ctx, htok_t expected) {
	if (ctx->parser.tok.type != expected) {
		hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected `%s`, expected `%s`", htok_name(ctx->parser.tok.type), htok_name(expected));
	}
}

static void hparser_expect_next(hcc_ctx_t *ctx, htok_t expected) {
	if (hlex_is_eof(&ctx->parser.lex_c)) {
		hcc_err(ctx, "unexpected EOF, expected `%s`", htok_name(expected));
	}
	hparser_next(ctx);
	if (ctx->parser.tok.type != expected) {
		hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected `%s`, expected `%s`", htok_name(ctx->parser.tok.type), htok_name(expected));
	}
}

static void hparser_fn_body_stmt(hcc_ctx_t *ctx, hproc_t *proc) {

}

static void hparser_fn_body(hcc_ctx_t *ctx, hproc_t *proc) {
	do {
		hlex_token_t ltoken = ctx->parser.tok;
		if (!hparser_next_if_not_eof(ctx)) {
			hcc_err_with_pos(ctx, ltoken, "unexpected EOF after token, expected `}`");
		}
		hparser_fn_body_stmt(ctx, proc);
	} while (ctx->parser.tok.type != htok_cbrace);
}

static void hparser_fn_asm_body(hcc_ctx_t *ctx, hproc_t *proc) {
	assert(0 && "TODO: not implemented");
}

static void hparser_fn_stmt(hcc_ctx_t *ctx) {
	hproc_t proc = {};

	if (ctx->parser.tok.type == htok_extern) {
		proc.is_extern = true;
		hparser_expect_next(ctx, htok_fn);
	}

	assert(ctx->parser.tok.type == htok_fn);

	hparser_expect_next(ctx, htok_ident);
	hlex_token_t fn_name = ctx->parser.tok;
	hparser_expect_next(ctx, htok_opar);
	hparser_next(ctx);

	// parse type list
	while (ctx->parser.tok.type != htok_cpar) {
		hparser_expect(ctx, htok_ident);
		hlex_token_t arg_name = ctx->parser.tok;
		hparser_expect_next(ctx, htok_colon);
		hparser_expect_next(ctx, htok_ident);
		hlex_token_t arg_type = ctx->parser.tok;

		// debug dump
		printf("arg {\n");
		hlex_token_dump(arg_name);
		hlex_token_dump(arg_type);
		printf("}\n");

		/* hproc_arg_t arg = {
			.name = arg_name,
			.type = arg_type,
		};

		hvec_push(&proc.args, arg); */

		hparser_next(ctx);
		if (ctx->parser.tok.type == htok_comma) {
			hparser_next(ctx);
		}
	}

	if (hparser_next_if_not_eof(ctx) && ctx->parser.tok.type == htok_colon) {
		hparser_expect_next(ctx, htok_ident);
		hlex_token_t ret_type = ctx->parser.tok;

		printf("retarg {\n");
		hlex_token_dump(ret_type);
		printf("}\n");
	}

	// has body
	if (hparser_next_if_not_eof(ctx)) {
		if (ctx->parser.tok.type == htok_obrace) {
			// body
			hparser_fn_body(ctx, &proc);
		} else if (ctx->parser.tok.type == htok_asm) {
			// asm body
			hparser_fn_asm_body(ctx, &proc);
		} else {
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected `%s`, expected `{` or `;`", htok_name(ctx->parser.tok.type));
		}
	}
}

static void hparser_top_stmt(hcc_ctx_t *ctx) {
	hlex_token_t token = ctx->parser.tok;

	switch (token.type) {
		case htok_fn:
		case htok_extern:
			hparser_fn_stmt(ctx);
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