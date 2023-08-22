#pragma once

#include "shared.h"

#include <setjmp.h>

typedef struct hlex_t hlex_t;
typedef struct htoken_t htoken_t;
typedef enum htok_t htok_t;

typedef struct hcc_ctx_t hcc_ctx_t; // forward declaration

void hlex_init(hlex_t *lex, u8 *pc, size_t plen);
htoken_t hlex_next(hlex_t *lex, hcc_ctx_t *ctx);
bool hlex_is_eof(hlex_t *lex);
const char *htok_name(htok_t tok);
void hlex_token_dump(htoken_t token);

struct hlex_t {
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	jmp_buf lex_err;
	char last_err_msg[128];
};

enum htok_t {
    #define X(name, _) name,
    #include "tok.def"
    #undef X
};

struct htoken_t {
	htok_t type;
	u32 row;
	u32 col;
	u32 len;
	u8 *p;
};