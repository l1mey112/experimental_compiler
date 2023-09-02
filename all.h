#pragma once

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <stdbool.h>
#include <assert.h>
#include <setjmp.h>
#include <string.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

#ifdef NDEBUG
	#define assert_not_reached()  __builtin_unreachable()
#else
	#define assert_not_reached()  assert(0 && "assert_not_reached")
#endif

#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define MAYBE_UNUSED __attribute__((unused))
#define ARRAYLEN(v) ((u32)(sizeof(v) / sizeof(*(v))))

static inline bool sv_cmp(u8 *a, size_t alen, u8 *b, size_t blen) {
	if (alen != blen) {
		return false;
	}
	return memcmp(a, b, alen) == 0;
}

static inline u32 ptrcpy(u8 *p, u8 *q, u32 len) {
	memcpy(p, q, len);
	return len;
}

#define sv_cmp_literal(a, alen, b) sv_cmp(a, alen, (u8 *)b, sizeof(b "") - 1)

typedef u32 rstr_t;
typedef u16 rfile_t;
typedef u16 type_t;
typedef struct typeinfo_t typeinfo_t;
typedef enum typeinfo_kind_t typeinfo_kind_t;
typedef struct token_t token_t;
typedef struct loc_t loc_t;
typedef struct file_entry_t file_entry_t;
typedef struct err_diag_t err_diag_t;
typedef enum tok_t tok_t;

rstr_t sv_intern(u8 *sv, size_t len);
const char *sv_from(rstr_t str);

bool file_slurp(FILE* file, const char *fp, rfile_t *handle);
void file_parse(rfile_t file);

// TODO: make smaller?
struct loc_t {
	u32 line_nr;
	u32 col;
	u32 pos;
	u16 len;
	rfile_t file;
};

struct file_entry_t {
	const char *fp;
	u8 *data;
	size_t len;
};

struct err_diag_t {
	jmp_buf unwind;
	char err_string[256];
	// TODO: more err information
	// loc_t err_loc;
};

extern size_t file_entry_count;
extern file_entry_t file_entries[256];
extern err_diag_t err_diag;

void err_with_pos(loc_t loc, const char *fmt, ...)
	__attribute__((format(printf, 2, 3))) __attribute__ ((__noreturn__));

void err_without_pos(const char *fmt, ...)
	__attribute__((format(printf, 1, 2))) __attribute__ ((__noreturn__));

#define TOK_X_KEYWORDS_LIST \
	X(TOK_FN, "fn") \
	X(TOK_EXTERN, "extern") \
	X(TOK_PURE, "pure") \
	X(TOK_ASM, "asm") \
	X(TOK_AS, "as") \
	X(TOK_RETURN, "return") \
	X(TOK_MUT, "mut") \
	X(TOK_IF, "if") \
	X(TOK_ELSE, "else") \
	X(TOK_FOR, "for") \
	X(TOK_BREAK, "break") \
	X(TOK_CONTINUE, "continue")

// in specific order due to how operators are parsed
#define TOK_X_OPERATOR_LIST \
	X(TOK_INC, "++") \
	X(TOK_ASSIGN_ADD, "+=") \
	X(TOK_ADD, "+") \
	X(TOK_ASSIGN_SUB, "-=") \
	X(TOK_DEC, "--") \
	X(TOK_SUB, "-") \
	X(TOK_ASSIGN_MUL, "*=") \
	X(TOK_MUL, "*") \
	X(TOK_ASSIGN_DIV, "/=") \
	X(TOK_DIV, "/") \
	X(TOK_ASSIGN_MOD, "%=") \
	X(TOK_MOD, "%") \
	X(TOK_EQ, "==") \
	X(TOK_ASSIGN, "=") \
	X(TOK_NEQ, "!=") \
	X(TOK_NOT, "!") \
	X(TOK_LSHIFT, "<<") \
	X(TOK_RSHIFT, ">>") \
	X(TOK_RUSHIFT, ">>>") \
	X(TOK_LE, "<=") \
	X(TOK_LT, "<") \
	X(TOK_GE, ">=") \
	X(TOK_GT, ">") \
	X(TOK_AND, "&&") \
	X(TOK_OR, "||") \
	X(TOK_BAND, "&") \
	X(TOK_BOR, "|") \
	X(TOK_XOR, "^") \
	X(TOK_TILDE, "~") \
	X(TOK_DOT, ".") \
	X(TOK_COMMA, ",") \
	X(TOK_OPAR, "(") \
	X(TOK_CPAR, ")") \
	X(TOK_OSQ, "[") \
	X(TOK_CSQ, "]") \
	X(TOK_OBRACE, "{") \
	X(TOK_CBRACE, "}") \
	X(TOK_COLON, ":") \
	X(TOK_QUESTION, "?")

#define TOK_HAS_LIT(t) \
	((t) == TOK_IDENT || \
	(t) == TOK_INTEGER)

#define TOK_IS_PREFIX(t) \
	((t) == TOK_SUB || \
	(t) == TOK_NOT || \
	(t) == TOK_TILDE || \
	(t) == TOK_MUL || \
	(t) == TOK_BAND)

#define TOK_IS_INFIX(t) \
	((t) == TOK_ADD || \
	(t) == TOK_SUB || \
	(t) == TOK_MUL || \
	(t) == TOK_DIV || \
	(t) == TOK_MOD || \
	(t) == TOK_ASSIGN || \
	(t) == TOK_ASSIGN_ADD || \
	(t) == TOK_ASSIGN_SUB || \
	(t) == TOK_ASSIGN_MUL || \
	(t) == TOK_ASSIGN_DIV || \
	(t) == TOK_ASSIGN_MOD || \
	(t) == TOK_EQ || \
	(t) == TOK_NEQ || \
	(t) == TOK_LT || \
	(t) == TOK_GT || \
	(t) == TOK_LE || \
	(t) == TOK_GE || \
	(t) == TOK_AND || \
	(t) == TOK_OR || \
	(t) == TOK_BAND || \
	(t) == TOK_BOR || \
	(t) == TOK_XOR || \
	(t) == TOK_LSHIFT || \
	(t) == TOK_RSHIFT || \
	(t) == TOK_RUSHIFT || \
	(t) == TOK_DOT || \
	(t) == TOK_AS)

#define TOK_X_LIST \
	X(TOK_UNDEFINED, "tok_undefined") \
	X(TOK_EOF, "EOF") \
	X(TOK_IDENT, "identifier") \
	X(TOK_INTEGER, "integer") \
	TOK_X_KEYWORDS_LIST \
	TOK_X_OPERATOR_LIST

enum tok_t {
    #define X(name, _) name,
    TOK_X_LIST
    #undef X
};

struct token_t {
	tok_t type;
	loc_t loc;
	rstr_t lit;
};

const char *tok_dbg_str(token_t tok);

// scratch buffer for small allocations (string concatenation, etc)
u8 *alloc_scratch(size_t size);
void alloc_reset(u8 *p);

#define TYPE_X_CONCRETE_LIST \
	X(TYPE_VOID, "void") \
	X(TYPE_I8, "i8") \
	X(TYPE_I16, "i16") \
	X(TYPE_I32, "i32") \
	X(TYPE_I64, "i64") \
	X(TYPE_ISIZE, "isize") \
	X(TYPE_U8, "u8") \
	X(TYPE_U16, "u16") \
	X(TYPE_U32, "u32") \
	X(TYPE_U64, "u64") \
	X(TYPE_USIZE, "usize") \
	X(TYPE_F32, "f32") \
	X(TYPE_F64, "f64") \
	X(TYPE_BOOL, "bool") \
	X(TYPE_NORETURN, "noreturn")

// types
enum typeinfo_kind_t {
	#define X(name, _) name,
    TYPE_X_CONCRETE_LIST
    #undef X
	_TYPE_CONCRETE_MAX,
	//
	TYPE_UNKNOWN,
	TYPE_FN,
	TYPE_PTR,
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FN_PTR,
	// TYPE_STRUCT,
	// TYPE_FIXEDARRAY,
};

// TODO: impl scratch arena allocator for unchanging arrays

struct typeinfo_t {
	typeinfo_kind_t kind;

	union {
		struct {
			rstr_t lit;
		} d_unknown;
		struct {
			type_t *args;
			type_t *rets;
			u8 args_len;
			u8 rets_len;
		} d_fn;
		type_t type_ref;
	};

	/* union {
		struct {
			bool atomic : 1;
			bool nullable : 1;
		};
		u8 flags;
	}; */
};

type_t table_new(typeinfo_t typeinfo);
typeinfo_t *table_get(type_t type);
type_t table_new_inc_mul(type_t type);
const char *table_type_dbg_str(type_t type);