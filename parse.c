#include <stdio.h>

#include "ir.h"
#include "parse.h"
#include "hasc.h"
#include "lex.h"
#include "shared.h"
#include "stb_ds.h"
#include "type.h"

void hparser_init(hcc_ctx_t *ctx, u8 *pc, size_t plen) {
	hlex_init(&ctx->parser.lex_c, pc, plen);
}

static void hparser_next(hcc_ctx_t *ctx) {
	ctx->parser.tok = hlex_next(&ctx->parser.lex_c, ctx);
	// hlex_token_dump(ctx->parser.tok);
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
	while (true) {
		htoken_t ltoken = ctx->parser.tok;
		if (!hparser_next_if_not_eof(ctx)) {
			hcc_err_with_pos(ctx, ltoken, "unexpected EOF after token, expected `}`");
		}
		if (ctx->parser.tok.type == HTOK_CBRACE) {
			return;
		}
		hparser_fn_body_stmt(ctx, proc);
	}
}

static void hparser_fn_asm_body(hcc_ctx_t *ctx, hproc_t *proc) {
	assert(0 && "TODO: not implemented");
}

// WARNING: will call next
static htype_t hparser_parse_type(hcc_ctx_t *ctx) {
	htoken_t tok = ctx->parser.tok;

	if (hlex_is_eof(&ctx->parser.lex_c)) {
		hcc_err_with_pos(ctx, tok, "unexpected EOF while parsing type");
	}

	hparser_next(ctx); // safe to call next()
	tok = ctx->parser.tok;

	switch (ctx->parser.tok.type) {		
		case HTOK_MUL:
			return htable_type_inc_muls(ctx, hparser_parse_type(ctx));
		case HTOK_QUESTION:
			return htable_type_option_of(ctx, hparser_parse_type(ctx));
		case HTOK_IDENT:
			return htable_type_get(ctx, tok);
		default:
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected token `%s` inside type", htok_name(ctx->parser.tok.type));
	}
}

static void hparser_fn_stmt(hcc_ctx_t *ctx) {
	hproc_t proc = {};
	htypeinfo_t fn_type;

	if (ctx->parser.tok.type == HTOK_EXTERN) {
		proc.is_extern = true;
		hparser_expect_next(ctx, HTOK_FN);
	}

	assert(ctx->parser.tok.type == HTOK_FN);

	hparser_expect_next(ctx, HTOK_IDENT);
	htoken_t fn_name = ctx->parser.tok;
	hparser_expect_next(ctx, HTOK_OPAR);
	hparser_next(ctx);

	// NOTE: you are using a scratch buffer, this is fucking unsafe!
	//       make your prayers that the type function still clones

	int scratch_buf_len = 0;
	static htoken_t scratch_buf_args[128];
	static htype_t scratch_buf_types[128];

	_Static_assert(ARRAYSIZE(scratch_buf_args) == ARRAYSIZE(scratch_buf_types), "uhh");

	// parse type list
	while (ctx->parser.tok.type != HTOK_CPAR) {
		hparser_expect(ctx, HTOK_IDENT);
		htoken_t arg_name = ctx->parser.tok;
		hparser_expect_next(ctx, HTOK_COLON);

		htype_t type = hparser_parse_type(ctx);

		assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
		scratch_buf_args[scratch_buf_len] = arg_name;
		scratch_buf_types[scratch_buf_len] = type;
		scratch_buf_len++;

		hparser_next(ctx);
		if (ctx->parser.tok.type == HTOK_COMMA) {
			hparser_next(ctx);
		}
	}

	// TODO: scratch_buf_args, remember names..

	fn_type.type = HT_FN;
	fn_type.d_fn.args = scratch_buf_types;
	fn_type.d_fn.args_len = scratch_buf_len;
	fn_type.d_fn.rets = NULL;
	fn_type.d_fn.rets_len = 0;

	if (hparser_next_if_not_eof(ctx) && ctx->parser.tok.type == HTOK_COLON) {
		int scratch_buf_len_start = scratch_buf_len;

		htype_t type = hparser_parse_type(ctx);

		// TODO: multireturn
		assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
		scratch_buf_types[scratch_buf_len] = type;
		scratch_buf_len++;

		fn_type.d_fn.rets = scratch_buf_types + scratch_buf_len_start;
		fn_type.d_fn.rets_len = scratch_buf_len - scratch_buf_len_start;
	}

	proc.fn_type = htable_intern_append(ctx, fn_type);
	htable_type_dump(ctx, proc.fn_type);

	// has body
	if (hparser_next_if_not_eof(ctx)) {
		if (ctx->parser.tok.type == HTOK_OBRACE) {
			// body
			hparser_fn_body(ctx, &proc);
		} else if (ctx->parser.tok.type == HTOK_ASM) {
			// asm body
			hparser_fn_asm_body(ctx, &proc);
		} else {
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected `%s`, expected `{` or `;`", htok_name(ctx->parser.tok.type));
		}
	}
}

static void hparser_top_stmt(hcc_ctx_t *ctx) {
	htoken_t token = ctx->parser.tok;

	switch (token.type) {
		case HTOK_FN:
		case HTOK_EXTERN:
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