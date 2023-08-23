#pragma once

#include "lex.h"

typedef struct hparser_t hparser_t;

struct hparser_t {
	hlex_t lex_c;
	htoken_t tok;
	//
	struct {
		u32 start;
		u32 end;
	} scope_spans[128];
	u32 scope_spans_len;
};

typedef struct hcc_ctx_t hcc_ctx_t; // forward declaration

void hparser_init(hcc_ctx_t *ctx, u8 *pc, size_t plen);
void hparser_run(hcc_ctx_t *ctx);