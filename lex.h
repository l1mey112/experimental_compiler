#include "shared.h"

#include <setjmp.h>

typedef struct hlex_t hlex_t;
typedef struct hlex_token_t hlex_token_t;
typedef enum htok_t htok_t;

void hlex_init(hlex_t *lex, u8 *pc, size_t plen);
hlex_token_t hlex_next(hlex_t *lex);
bool hlex_is_eof(hlex_t *lex);

struct hlex_t {
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	jmp_buf lex_err;
	// TODO: last err char*
};

enum htok_t {
    htok_add,
    htok_sub,
    htok_mul,
    htok_div,
    htok_mod,
    htok_inc,
    htok_dec,
    htok_assign,
    htok_not,
    htok_eq,
    htok_neq,
    htok_lt,
    htok_gt,
    htok_and,
    htok_or,
    htok_band,
    htok_bor,
    htok_xor,
    htok_lshift,
    htok_rshift,
    htok_rushift,
    htok_tilde,
    htok_dot,
    htok_comma,
    htok_opar,
    htok_cpar,
    htok_osq,
    htok_csq,
    htok_obrace,
    htok_cbrace,
    htok_ident,
    htok_integer,
};

struct hlex_token_t {
	htok_t type;
	u32 row;
	u32 col;
	u32 len;
	u8 *p;
};
