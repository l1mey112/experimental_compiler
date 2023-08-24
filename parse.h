#pragma once

#include "ir.h"
#include "tok.h"

typedef struct hparser_t hparser_t;

struct hparser_t {
	struct lexer {
		u8 *pc;
		u8 *pend;
		u8 *plast_nl;
		u32 line_nr;
	} lex;
	htoken_t tok;
	htoken_t peek;
	//
	struct {
		u32 start;
		u32 end;
	} scope_spans[128];
	u32 scope_spans_len;
};

typedef struct hcc_ctx_t hcc_ctx_t; // forward declaration

void hparser_init(hcc_ctx_t *ctx, hparser_t *parser, u8 *pc, size_t plen);
void hparser_run(hcc_ctx_t *ctx, hparser_t *parser);
void hparser_next(hcc_ctx_t *ctx, hparser_t *parser);

htoken_t hparser_lex_next(hparser_t *parser, hcc_ctx_t *ctx);
bool hparser_lex_is_eof(hparser_t *parser);
