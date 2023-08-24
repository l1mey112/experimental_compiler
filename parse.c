#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "parse.h"
#include "hasc.h"
#include "lex.h"
#include "shared.h"
#include "stb_ds.h"
#include "type.h"

void hparser_init(hcc_ctx_t *ctx, hparser_t *parser, u8 *pc, size_t plen) {
	hlex_init(&parser->lex_c, pc, plen);
}

static void hparser_next(hcc_ctx_t *ctx, hparser_t *parser) {
	parser->tok = hlex_next(&parser->lex_c, ctx);
	// hlex_token_dump(parser->tok);
}

static bool hparser_next_if_not_eof(hcc_ctx_t *ctx, hparser_t *parser) {
	if (hlex_is_eof(&parser->lex_c)) {
		return false;
	}
	hparser_next(ctx, parser);
	return true;
}

static void hparser_expect(hcc_ctx_t *ctx, hparser_t *parser, htok_t expected) {
	if (parser->tok.type != expected) {
		hcc_err_with_pos(ctx, parser->tok, "unexpected `%s`, expected `%s`", htok_name(parser->tok.type), htok_name(expected));
	}
}

static void hparser_expect_next(hcc_ctx_t *ctx, hparser_t *parser, htok_t expected) {
	if (hlex_is_eof(&parser->lex_c)) {
		hcc_err(ctx, "unexpected EOF, expected `%s`", htok_name(expected));
	}
	hparser_next(ctx, parser);
	if (parser->tok.type != expected) {
		hcc_err_with_pos(ctx, parser->tok, "unexpected `%s`, expected `%s`", htok_name(parser->tok.type), htok_name(expected));
	}
}

static void hparser_expect_next_not_eof(hcc_ctx_t *ctx, hparser_t *parser) {
	if (hlex_is_eof(&parser->lex_c)) {
		hcc_err(ctx, "unexpected EOF");
	}
	hparser_next(ctx, parser);
}

static u32 hparser_new_cfg_node(hcc_ctx_t *ctx, hparser_t *parser) {
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

static u32 hparser_new_ast_node(hcc_ctx_t *ctx, hparser_t *parser, hast_type_t type) {
	u32 nidx = stbds_arrlenu(ctx->arena.ast_arena);
	hast_node_t node = {
		.type = type,
		.children = {-1, -1, -1},
		.next = -1,
	};
	stbds_arrpush(ctx->arena.ast_arena, node);
	return nidx;
}

static u32 hcc_ast_node_from_literal(hcc_ctx_t *ctx, hparser_t *parser, htoken_t literal, bool negate) {
	assert(literal.type == HTOK_INTEGER); // TODO: float values...

	static char scratch_buf[128];
	assert(literal.len < 127 && "TODO: you shouldn't be doing this, but l-m needs to create a strtol function that works with SVs");

	memcpy(scratch_buf, literal.p, literal.len);
	scratch_buf[literal.len] = '\0';

	u32 node = hparser_new_ast_node(ctx, parser, HAST_EXPR_LITERAL_INT);
	hast_node_t *nodep = hcc_ast_node(ctx, node);

	nodep->d_literal_int.value = strtol(scratch_buf, NULL, 10);
	nodep->token = literal;

	if (errno == ERANGE) {
		hcc_err_with_pos(ctx, literal, "integer out of range");
	}

	if (negate) {
		nodep->d_literal_int.value = -nodep->d_literal_int.value;
	}

	return node;
}

// returns `-1` on error
static u32 hparser_search_scope(hcc_ctx_t *ctx, hparser_t *parser, htoken_t token) {
	assert(parser->scope_spans_len > 0);

	size_t hash = hsv_name_hash(token.p, token.len);

	for (u32 p = parser->scope_spans_len; p > 0;) {
		p--;
		// no variable shadowing within same scope
		u32 start = parser->scope_spans[p].start;
		u32 end = parser->scope_spans[p].end;
		for (u32 i = start; i < end; i++) {
			if (ctx->current_proc->locals[i].name_hash == hash) {
				return i;
			}
		}
	}

	return -1;
}

static void hparser_push_scope(hcc_ctx_t *ctx, hparser_t *parser) {
	if (parser->scope_spans_len >= ARRAYSIZE(parser->scope_spans)) {
		hcc_err(ctx, "indentations higher than 128? what is wrong with you?");
	}
	u32 start = stbds_arrlenu(ctx->current_proc);
	parser->scope_spans[parser->scope_spans_len].start = start;
	parser->scope_spans[parser->scope_spans_len].end = start;
	parser->scope_spans_len++;
}

enum {
	PREC_UNKOWN,  // default
	PREC_ASSIGN,  // = += -= *= /= %=
	PREC_CMP,     // && || (TODO: needs parens)
	PREC_EQ,      // == != < > <= >=
	PREC_BOR,     // |
	PREC_XOR,     // ^
	PREC_BAND,    // &
	PREC_ADD,     // + -
	PREC_MUL,     // * / %
	PREC_AS_CAST, // as
	PREC_PREFIX,  // - * ! &
	PREC_POSTFIX, // ++ --
	PREC_CALL,    // .x x() x[]
};

static u8 hparser_tok_precedence(htok_t type) {
	// PREC_PREFIX is determined elsewhere

	switch (type) {
		case HTOK_DOT:
		case HTOK_OSQ:
		case HTOK_OPAR:
			return PREC_CALL;
		case HTOK_INC:
		case HTOK_DEC:
			return PREC_POSTFIX;
		case HTOK_AS:
			return PREC_AS_CAST;
		case HTOK_MUL:
		case HTOK_DIV:
		case HTOK_MOD:
			return PREC_MUL;
		case HTOK_ADD:
		case HTOK_SUB:
			return PREC_ADD;
		case HTOK_BAND:
			return PREC_BAND;
		case HTOK_XOR:
			return PREC_XOR;
		case HTOK_BOR:
			return PREC_BOR;
		case HTOK_EQ:
		case HTOK_NEQ:
		case HTOK_LT:
		case HTOK_GT:
		case HTOK_GE:
		case HTOK_LE:
			return PREC_EQ;
		case HTOK_AND:
		case HTOK_OR:
			return PREC_CMP;
		case HTOK_ASSIGN:
		case HTOK_ASSIGN_ADD:
		case HTOK_ASSIGN_SUB:
		case HTOK_ASSIGN_MUL:
		case HTOK_ASSIGN_DIV:
		case HTOK_ASSIGN_MOD:
			return PREC_ASSIGN;
		default:
			return 0;
	}
}

static u32 hparser_expr_next(hcc_ctx_t *ctx, hparser_t *parser, u8 prec) {
	u32 node;
	hast_node_t *n;

	switch (parser->tok.type) {
		case HTOK_IDENT: {
			node = hparser_new_ast_node(ctx, parser, HAST_EXPR_IDENT);
			n = hcc_ast_node(ctx, node);
			u32 idx = hparser_search_scope(ctx, parser, parser->tok);
			n->d_ident.is_local = idx != (u32)-1;
			n->d_ident.idx = idx; // -1 on not found
			n->token = parser->tok;
			hparser_next_if_not_eof(ctx, parser);
			break;
		}
		case HTOK_OPAR: {
			hparser_expect_next_not_eof(ctx, parser);
			node = hparser_expr_next(ctx, parser, 0);
			hparser_expect(ctx, parser, HTOK_CPAR);
			hparser_next_if_not_eof(ctx, parser); // see 1:
			break;
		}
		case HTOK_INTEGER: {
			node = hcc_ast_node_from_literal(ctx, parser, parser->tok, false);
			hparser_next_if_not_eof(ctx, parser); // see 1:
			break;
		}
		default: {
			htok_t t = parser->tok.type;
			
			if (HTOK_IS_PREFIX(t)) {
				htoken_t token = parser->tok;
				hparser_expect_next_not_eof(ctx, parser);

				// TODO: floats...
				if (t == HTOK_SUB && parser->tok.type == HTOK_INTEGER) {
					node = hcc_ast_node_from_literal(ctx, parser, parser->tok, true);
					hparser_next_if_not_eof(ctx, parser); // see 1:
				} else {
					node = hparser_new_ast_node(ctx, parser, HAST_EXPR_PREFIX);
					u32 next = hparser_expr_next(ctx, parser, PREC_PREFIX);
					n = hcc_ast_node(ctx, node);
					n->token = token;
					n->children[0] = next;
				}
			} else {
				hcc_err_with_pos(ctx, parser->tok, "unexpected `%s` in prefix position", htok_name(t));
			}
			break;
		}
	}

	// 1: move the lexer along into either the next token state, or an EOF
	if (hlex_is_eof(&parser->lex_c)) {
		return node;
	}

	// hlex_token_dump(parser->tok);
	while (prec < hparser_tok_precedence(parser->tok.type)) {
		htoken_t token = parser->tok;
		
		switch (token.type) {
			default: {
				if (HTOK_IS_INFIX(token.type)) {
					u8 prec = hparser_tok_precedence(parser->tok.type);

					// next()
					hparser_expect_next_not_eof(ctx, parser);

					u32 lhs = node;
					u32 rhs = hparser_expr_next(ctx, parser, prec);

					u32 expr = hparser_new_ast_node(ctx, parser, HAST_EXPR_INFIX);
					n = hcc_ast_node(ctx, expr);
					n->token = token;
					n->children[0] = lhs;
					n->children[1] = rhs;

					node = expr;
				} else {
					printf("exit!\n");
					hlex_token_dump(token);
					goto exit;
				}
				break;
			}
		}
	}
exit:

	return node;
}

static u32 hparser_fn_body_stmt_next(hcc_ctx_t *ctx, hparser_t *parser) {
	switch (parser->tok.type) {
		default: {
			u32 node = hparser_new_ast_node(ctx, parser, HAST_STMT_EXPR);
			u32 next = hparser_expr_next(ctx, parser, 0);
			hast_node_t *n = hcc_ast_node(ctx, node);
			n->children[0] = next;
			return node;
		}
	}

	assert_not_reached();
}

// TODO: refactor to parse just a {} and store the scope stuff here
static void hparser_body(hcc_ctx_t *ctx, hparser_t *parser, u32 cfg_node) {
	assert(parser->tok.type == HTOK_OBRACE);

	hcfg_node_t *cfg = hcc_cfg_node(ctx, cfg_node);
	u32 current_ast = cfg->ast_begin;

	hparser_expect_next_not_eof(ctx, parser);

	while (true) {
		if (parser->tok.type == HTOK_CBRACE) {
			return;
		}

		u32 node = hparser_fn_body_stmt_next(ctx, parser);
		printf("node: %u, ch: %u\n", node, hcc_ast_node(ctx, node)->children[0]);

		// cfg is empty
		if (current_ast == (u32)-1) {
			cfg = hcc_cfg_node(ctx, cfg_node);
			cfg->ast_begin = node;
		} else {
			hast_node_t *current_nodep = hcc_ast_node(ctx, current_ast);
			current_nodep->next = node;
		}

		current_ast = node;
	}
}

static void hparser_fn_asm_body(hcc_ctx_t *ctx, hparser_t *parser, hproc_t *proc) {
	(void)ctx;
	(void)proc;
	assert(0 && "TODO: not implemented");
}

// WARNING: will call next
static htype_t hparser_next_parse_type(hcc_ctx_t *ctx, hparser_t *parser) {
	htoken_t tok = parser->tok;

	if (hlex_is_eof(&parser->lex_c)) {
		hcc_err_with_pos(ctx, tok, "unexpected EOF while parsing type");
	}

	hparser_next(ctx, parser); // safe to call next()
	tok = parser->tok;

	switch (parser->tok.type) {		
		case HTOK_MUL:
			return htable_type_inc_muls(ctx, hparser_next_parse_type(ctx, parser));
		case HTOK_QUESTION:
			return htable_type_option_of(ctx, hparser_next_parse_type(ctx, parser));
		case HTOK_IDENT:
			return htable_type_get(ctx, tok);
		default:
			hcc_err_with_pos(ctx, parser->tok, "unexpected token `%s` inside type", htok_name(parser->tok.type));
	}
}

static htype_t hparser_parse_type(hcc_ctx_t *ctx, hparser_t *parser) {
	htoken_t tok = parser->tok;

	switch (parser->tok.type) {		
		case HTOK_MUL:
			return htable_type_inc_muls(ctx, hparser_next_parse_type(ctx, parser));
		case HTOK_QUESTION:
			return htable_type_option_of(ctx, hparser_next_parse_type(ctx, parser));
		case HTOK_IDENT:
			return htable_type_get(ctx, tok);
		default:
			hcc_err_with_pos(ctx, parser->tok, "unexpected token `%s` inside type", htok_name(parser->tok.type));
	}
}

static void hparser_pop_scope(hcc_ctx_t *ctx, hparser_t *parser) {
	assert(parser->scope_spans_len > 0);
	parser->scope_spans_len--;
}

static void hparser_fn_stmt(hcc_ctx_t *ctx, hparser_t *parser) {
	hproc_t proc = {};
	htypeinfo_t fn_type;

	if (parser->tok.type == HTOK_EXTERN) {
		proc.is_extern = true;
		hparser_expect_next(ctx, parser, HTOK_FN);
	}

	assert(parser->tok.type == HTOK_FN);

	hparser_expect_next(ctx, parser, HTOK_IDENT);
	htoken_t fn_name = parser->tok;
	proc.fn_name = fn_name;
	hparser_expect_next(ctx, parser, HTOK_OPAR);
	hparser_next(ctx, parser);

	// NOTE: you are using a scratch buffer, this is fucking unsafe!
	//       make your prayers that the type function still clones

	u32 scratch_buf_len = 0;
	static htoken_t scratch_buf_args[128];
	static htype_t scratch_buf_types[128];

	_Static_assert(ARRAYSIZE(scratch_buf_args) == ARRAYSIZE(scratch_buf_types), "uhh");

	// parse type list
	while (parser->tok.type != HTOK_CPAR) {
		hparser_expect(ctx, parser, HTOK_IDENT);
		htoken_t arg_name = parser->tok;
		hparser_expect_next(ctx, parser, HTOK_COLON);

		htype_t type = hparser_next_parse_type(ctx, parser);

		for (u32 i = 0; i < scratch_buf_len; i++) {
			if (hsv_memcmp(arg_name.p, arg_name.len, scratch_buf_args[i].p, scratch_buf_args[i].len)) {
				hcc_err_with_pos(ctx, arg_name, "duplicate function parameter name");
			}
		}

		assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
		scratch_buf_args[scratch_buf_len] = arg_name;
		scratch_buf_types[scratch_buf_len] = type;
		scratch_buf_len++;

		hparser_next(ctx, parser);
		if (parser->tok.type == HTOK_COMMA) {
			hparser_next(ctx, parser);
		}
	}

	u32 scratch_buf_args_len = scratch_buf_len;
	fn_type.type = HT_FN;
	fn_type.d_fn.args = scratch_buf_types;
	fn_type.d_fn.args_len = scratch_buf_len;
	fn_type.d_fn.rets = NULL;
	fn_type.d_fn.rets_len = 0;

	if (hparser_next_if_not_eof(ctx, parser) && parser->tok.type == HTOK_COLON) {
		u32 scratch_buf_len_start = scratch_buf_len;

		if (hlex_is_eof(&parser->lex_c)) {
			hcc_err_with_pos(ctx, parser->tok, "unexpected EOF while parsing type");
		}
		hparser_next(ctx, parser); // safe to call next()

		if (parser->tok.type == HTOK_OPAR) {
			if (hlex_is_eof(&parser->lex_c)) {
				hcc_err_with_pos(ctx, parser->tok, "unexpected EOF while parsing type");
			}
			hparser_next(ctx, parser); // safe to call next()
			while (parser->tok.type != HTOK_CPAR) {
				htype_t type = hparser_parse_type(ctx, parser);

				assert(scratch_buf_len < ARRAYSIZE(scratch_buf_args));
				scratch_buf_types[scratch_buf_len] = type;
				scratch_buf_len++;

				hparser_next(ctx, parser);
				if (parser->tok.type == HTOK_COMMA) {
					hparser_next(ctx, parser);
				}
			}
		} else {
			htype_t type = hparser_next_parse_type(ctx, parser);

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
	if (hparser_next_if_not_eof(ctx, parser)) {
		assert(parser->scope_spans_len == 0);
		hparser_push_scope(ctx, parser);
		parser->scope_spans[0].end = scratch_buf_args_len;

		if (parser->tok.type == HTOK_OBRACE) {
			// body
			u32 cfg_c = hparser_new_cfg_node(ctx, parser);
			ctx->current_proc->cfg_begin = cfg_c;
			hparser_body(ctx, parser, cfg_c);
		} else if (parser->tok.type == HTOK_ASM) {
			// asm body
			hparser_fn_asm_body(ctx, parser, &proc);
		} else {
			hcc_err_with_pos(ctx, parser->tok, "unexpected `%s`, expected `{` or `;`", htok_name(parser->tok.type));
		}

		hparser_pop_scope(ctx, parser);
	}
}

static void hparser_top_stmt(hcc_ctx_t *ctx, hparser_t *parser) {
	htoken_t token = parser->tok;

	switch (token.type) {
		case HTOK_FN:
		case HTOK_EXTERN:
			hparser_fn_stmt(ctx, parser);
			break;
		default:
			hcc_err_with_pos(ctx, token, "expected function or extern");
			break;
	}
}

void hparser_run(hcc_ctx_t *ctx, hparser_t *parser) {
	while(!hlex_is_eof(&parser->lex_c)) {
		hparser_next(ctx, parser); // TODO: what about whitespace at the EOF??? argh..
		hparser_top_stmt(ctx, parser);
	}
}
