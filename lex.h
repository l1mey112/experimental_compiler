#pragma once

#include "shared.h"

#include <setjmp.h>

typedef struct hlex_t hlex_t;
typedef struct hlex_token_t hlex_token_t;
typedef enum htok_t htok_t;

typedef struct hcc_ctx_t hcc_ctx_t; // forward declaration

void hlex_init(hlex_t *lex, u8 *pc, size_t plen);
hlex_token_t hlex_next(hlex_t *lex, hcc_ctx_t *ctx);
bool hlex_is_eof(hlex_t *lex);
const char *htok_name(htok_t tok);

struct hlex_t {
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	jmp_buf lex_err;
	char last_err_msg[128];
};

enum htok_t {
    #define X(name) name,
    #include "tok.def"
    #undef X
};

struct hlex_token_t {
	htok_t type;
	u32 row;
	u32 col;
	u32 len;
	u8 *p;
};
