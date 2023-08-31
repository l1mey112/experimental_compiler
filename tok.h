#pragma once

#include "shared.h"

#include <setjmp.h>

typedef struct hlex_t hlex_t;
typedef struct htoken_t htoken_t;
typedef enum htok_t htok_t;

const char *htok_name(htok_t tok);
void htoken_dump(htoken_t token);

enum htok_t {
	HTOK_UNKNOWN, // no token
	HTOK_EOF,     // no token
    #define X(name, _) name,
    #include "tok.def"
    #undef X
};

struct hloc_t {
	u32 line_nr
	u32 col
	u32 pos
	u16 len
	u16 file
}

struct htoken_t {
	htok_t type;
	hloc_t loc;
};

#define HTOK_IS_PREFIX(t) \
	(t) == HTOK_SUB || (t) == HTOK_NOT || (t) == HTOK_TILDE || (t) == HTOK_MUL || (t) == HTOK_BAND

#define HTOK_IS_INFIX(t) \
	(t) == HTOK_ADD || \
	(t) == HTOK_SUB || \
	(t) == HTOK_MUL || \
	(t) == HTOK_DIV || \
	(t) == HTOK_MOD || \
	(t) == HTOK_ASSIGN || \
	(t) == HTOK_ASSIGN_ADD || \
	(t) == HTOK_ASSIGN_SUB || \
	(t) == HTOK_ASSIGN_MUL || \
	(t) == HTOK_ASSIGN_DIV || \
	(t) == HTOK_ASSIGN_MOD || \
	(t) == HTOK_EQ || \
	(t) == HTOK_NEQ || \
	(t) == HTOK_LT || \
	(t) == HTOK_GT || \
	(t) == HTOK_LE || \
	(t) == HTOK_GE || \
	(t) == HTOK_AND || \
	(t) == HTOK_OR || \
	(t) == HTOK_BAND || \
	(t) == HTOK_BOR || \
	(t) == HTOK_XOR || \
	(t) == HTOK_LSHIFT || \
	(t) == HTOK_RSHIFT || \
	(t) == HTOK_RUSHIFT || \
	(t) == HTOK_DOT || \
	(t) == HTOK_AS
