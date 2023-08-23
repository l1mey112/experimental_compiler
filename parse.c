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

static inline hcfg_node_t *hparser_cfg_node(hcc_ctx_t *ctx, u32 node) {
	assert(node != -1);
	assert(node < stbds_arrlenu(ctx->arena.cfg_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.cfg_arena[node];
}

static inline hast_node_t *hparser_ast_node(hcc_ctx_t *ctx, u32 node) {
	assert(node != -1);
	assert(node < stbds_arrlenu(ctx->arena.ast_arena)); // TODO: reasonably safe? remove this?
	return &ctx->arena.ast_arena[node];
}

static u32 hparser_new_cfg_node(hcc_ctx_t *ctx) {
	u32 nidx = stbds_arrlenu(ctx->arena.cfg_arena);
	hcfg_node_t node;
	node.nidx = nidx;
	node.node_false = -1;
	node.node_true = -1;
	node.ast_begin = -1;
	node.ast_cond = -1;
	stbds_arrpush(ctx->arena.cfg_arena, node);
	return nidx;
}

static u32 hparser_new_ast_node(hcc_ctx_t *ctx, hast_type_t type) {
	u32 nidx = stbds_arrlenu(ctx->arena.cfg_arena);
	hast_node_t node = {
		.type = type,
		.children = {-1, -1, -1},
		.next = -1,
	};
	stbds_arrpush(ctx->arena.ast_arena, node);
	return nidx;
}

// returns `-1` on error
static u32 hparser_search_scope(hcc_ctx_t *ctx, htoken_t token) {
	assert(ctx->parser.scope_spans_len > 0);
	u32 scope_idx = stbds_arrlenu(ctx->parser.scope_spans) - 1;

	size_t hash = hsv_name_hash(token.p, token.len);

	for (u32 p = ctx->parser.scope_spans_len; p > 0;) {
		p--;
		// no variable shadowing within same scope
		u32 start = ctx->parser.scope_spans[p].start;
		u32 end = ctx->parser.scope_spans[p].end;
		for (u32 i = start; i < end; i++) {
			if (ctx->current_proc->locals[i].name_hash == hash) {
				return i;
			}
		}
	}

	return -1;
}

static void hparser_push_scope(hcc_ctx_t *ctx) {
	if (ctx->parser.scope_spans_len >= ARRAYSIZE(ctx->parser.scope_spans)) {
		hcc_err(ctx, "indentations higher than 128? what is wrong with you?");
	}
	u32 start = stbds_arrlenu(ctx->current_proc);
	ctx->parser.scope_spans[ctx->parser.scope_spans_len].start = start;
	ctx->parser.scope_spans[ctx->parser.scope_spans_len].end = start;
	ctx->parser.scope_spans_len++;
}

static u8 hparser_tok_precedence(htok_t type) {
	return 0;
}

static u32 hparser_expr(hcc_ctx_t *ctx, u8 prec) {
	u32 node;
	hast_node_t *n;

	switch (ctx->parser.tok.type) {
		case HTOK_IDENT: {
			node = hparser_new_ast_node(ctx, HAST_EXPR_IDENT);
			n = hparser_ast_node(ctx, node);
			u32 idx = hparser_search_scope(ctx, ctx->parser.tok);
			n->d_ident.is_local = idx != -1;
			n->d_ident.idx = idx; // -1 on not found
		}
		default: {
			assert_not_reached();
		}
	}

	while (prec < hparser_tok_precedence(ctx->parser.tok.type)) {
		// blah
	}

	return node;
}

static u32 hparser_fn_body_stmt(hcc_ctx_t *ctx) {
	switch (ctx->parser.tok.type) {
		default: {
			u32 node = hparser_new_ast_node(ctx, HAST_STMT_EXPR);
			hast_node_t *n = hparser_ast_node(ctx, node);
			n->children[0] = hparser_expr(ctx, 0);
		}
	}

	assert_not_reached();
}

static void hparser_fn_body(hcc_ctx_t *ctx) {
	while (true) {
		htoken_t ltoken = ctx->parser.tok;
		if (!hparser_next_if_not_eof(ctx)) {
			hcc_err_with_pos(ctx, ltoken, "unexpected EOF after token, expected `}`");
		}
		if (ctx->parser.tok.type == HTOK_CBRACE) {
			return;
		}
		hparser_fn_body_stmt(ctx);
	}
}

static void hparser_fn_asm_body(hcc_ctx_t *ctx, hproc_t *proc) {
	assert(0 && "TODO: not implemented");
}

// WARNING: will call next
static htype_t hparser_next_parse_type(hcc_ctx_t *ctx) {
	htoken_t tok = ctx->parser.tok;

	if (hlex_is_eof(&ctx->parser.lex_c)) {
		hcc_err_with_pos(ctx, tok, "unexpected EOF while parsing type");
	}

	hparser_next(ctx); // safe to call next()
	tok = ctx->parser.tok;

	switch (ctx->parser.tok.type) {		
		case HTOK_MUL:
			return htable_type_inc_muls(ctx, hparser_next_parse_type(ctx));
		case HTOK_QUESTION:
			return htable_type_option_of(ctx, hparser_next_parse_type(ctx));
		case HTOK_IDENT:
			return htable_type_get(ctx, tok);
		default:
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected token `%s` inside type", htok_name(ctx->parser.tok.type));
	}
}

static htype_t hparser_parse_type(hcc_ctx_t *ctx) {
	htoken_t tok = ctx->parser.tok;

	switch (ctx->parser.tok.type) {		
		case HTOK_MUL:
			return htable_type_inc_muls(ctx, hparser_next_parse_type(ctx));
		case HTOK_QUESTION:
			return htable_type_option_of(ctx, hparser_next_parse_type(ctx));
		case HTOK_IDENT:
			return htable_type_get(ctx, tok);
		default:
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected token `%s` inside type", htok_name(ctx->parser.tok.type));
	}
}

static void hparser_pop_scope(hcc_ctx_t *ctx) {
	assert(ctx->parser.scope_spans_len > 0);
	ctx->parser.scope_spans_len--;
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
	proc.fn_name = fn_name;
	hparser_expect_next(ctx, HTOK_OPAR);
	hparser_next(ctx);

	// NOTE: you are using a scratch buffer, this is fucking unsafe!
	//       make your prayers that the type function still clones

	u32 scratch_buf_len = 0;
	static htoken_t scratch_buf_args[128];
	static htype_t scratch_buf_types[128];

	_Static_assert(ARRAYSIZE(scratch_buf_args) == ARRAYSIZE(scratch_buf_types), "uhh");

	// parse type list
	while (ctx->parser.tok.type != HTOK_CPAR) {
		hparser_expect(ctx, HTOK_IDENT);
		htoken_t arg_name = ctx->parser.tok;
		hparser_expect_next(ctx, HTOK_COLON);

		htype_t type = hparser_next_parse_type(ctx);

		assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
		scratch_buf_args[scratch_buf_len] = arg_name;
		scratch_buf_types[scratch_buf_len] = type;
		scratch_buf_len++;

		hparser_next(ctx);
		if (ctx->parser.tok.type == HTOK_COMMA) {
			hparser_next(ctx);
		}
	}

	u32 scratch_buf_args_len = scratch_buf_len;
	fn_type.type = HT_FN;
	fn_type.d_fn.args = scratch_buf_types;
	fn_type.d_fn.args_len = scratch_buf_len;
	fn_type.d_fn.rets = NULL;
	fn_type.d_fn.rets_len = 0;

	if (hparser_next_if_not_eof(ctx) && ctx->parser.tok.type == HTOK_COLON) {
		u32 scratch_buf_len_start = scratch_buf_len;

		if (hlex_is_eof(&ctx->parser.lex_c)) {
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected EOF while parsing type");
		}
		hparser_next(ctx); // safe to call next()

		if (ctx->parser.tok.type == HTOK_OPAR) {
			if (hlex_is_eof(&ctx->parser.lex_c)) {
				hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected EOF while parsing type");
			}
			hparser_next(ctx); // safe to call next()
			while (ctx->parser.tok.type != HTOK_CPAR) {
				htype_t type = hparser_parse_type(ctx);

				assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
				scratch_buf_types[scratch_buf_len] = type;
				scratch_buf_len++;

				hparser_next(ctx);
				if (ctx->parser.tok.type == HTOK_COMMA) {
					hparser_next(ctx);
				}
			}
		} else {
			htype_t type = hparser_next_parse_type(ctx);

			assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
			scratch_buf_types[scratch_buf_len] = type;
			scratch_buf_len++;
		}
		
		fn_type.d_fn.rets = scratch_buf_types + scratch_buf_len_start;
		fn_type.d_fn.rets_len = scratch_buf_len - scratch_buf_len_start;
	}

	proc.fn_type = htable_intern_append(ctx, fn_type);
	htable_type_dump(ctx, proc.fn_type);

	for (u32 i = 0; i < scratch_buf_args_len; i++) {
		htoken_t token = scratch_buf_args[i];
		htype_t type = scratch_buf_types[i];
		hast_ident_t ident = {
			.is_arg = true,
			.name_hash = hsv_name_hash(token.p, token.len),
			.token = token,
			.type = type,
		};
		stbds_arrpush(proc.locals, ident);
	}

	u32 procs_len = stbds_arrlenu(ctx->procs);
	stbds_arrpush(ctx->procs, proc);
	ctx->current_proc = &ctx->procs[procs_len];

	ctx->current_proc->cfg_begin = -1;
	// has body
	if (hparser_next_if_not_eof(ctx)) {
		assert(ctx->parser.scope_spans_len == 0);
		hparser_push_scope(ctx);
		ctx->parser.scope_spans[0].end = scratch_buf_args_len;

		if (ctx->parser.tok.type == HTOK_OBRACE) {
			// body
			ctx->parser.cfg_c = hparser_new_cfg_node(ctx);
			hparser_fn_body(ctx);
		} else if (ctx->parser.tok.type == HTOK_ASM) {
			// asm body
			hparser_fn_asm_body(ctx, &proc);
		} else {
			hcc_err_with_pos(ctx, ctx->parser.tok, "unexpected `%s`, expected `{` or `;`", htok_name(ctx->parser.tok.type));
		}

		hparser_pop_scope(ctx);
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
