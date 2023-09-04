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

typedef u32 istr_t;
typedef u16 rfile_t;
typedef u16 type_t;
typedef struct typeinfo_t typeinfo_t;
typedef enum typeinfo_kind_t typeinfo_kind_t;
typedef struct token_t token_t;
typedef struct loc_t loc_t;
typedef struct file_entry_t file_entry_t;
typedef struct err_diag_t err_diag_t;
typedef enum tok_t tok_t;
typedef enum hir_inst_kind_t hir_inst_kind_t;
typedef struct hir_local_t hir_local_t;
typedef struct hir_block_t hir_block_t;
typedef struct hir_inst_t hir_inst_t;
typedef u32 hir_rlocal_t;
typedef u32 hir_rinst_t;
typedef u32 hir_rblock_t;

#define TYPE_UNRESOLVED ((type_t)-1)

istr_t sv_intern(u8 *sv, size_t len);
const char *sv_from(istr_t str);

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
	tok_t type; // TODO: this should be `kind` instead, types are types.
	loc_t loc;
	istr_t lit;
};

const char *tok_dbg_str(token_t tok);
const char *tok_literal_representation(tok_t tok);

// scratch buffer for small allocations (string concatenation, etc)
u8 *alloc_scratch(size_t size);
void alloc_reset(u8 *p);

// these will be matched in a hash table
#define TYPE_X_CONCRETE_LITERALS_LIST \
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

// `void` isn't a keyword.
#define TYPE_X_CONCRETE_LIST \
	TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_VOID, "void")

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

// will be filled from main()
extern istr_t typeinfo_concrete_istr[_TYPE_CONCRETE_MAX];
extern u32 typeinfo_concrete_istr_size;

struct typeinfo_t {
	typeinfo_kind_t kind;

	union {
		struct {
			istr_t lit;
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

// TODO: discrepancy
//       function arguments aren't locals in the normal sense, they're immutable value insts
//       their address (COULD?) be taken??
//       okay, fuck! their address is taken.
//       variables should be declared using new var to take part in liveness too

struct hir_local_t {
	istr_t name;
	loc_t name_loc;
	type_t type;
	loc_t type_loc;
	bool is_arg;
	hir_rinst_t inst;
};

struct hir_proc_t {
	istr_t name;
	loc_t name_loc;
	type_t type;
	u16 args;
	u16 rets;
	hir_rblock_t entry;
	hir_local_t *locals;
	hir_block_t *blocks;
	hir_inst_t *insts;
	bool is_extern;
};

struct hir_block_t {
	hir_rblock_t id;
	hir_rinst_t first;
	u32 len;
};

enum hir_inst_kind_t {
	HIR_NOP,
	HIR_ARG,
	HIR_LOCAL,
	HIR_LOCAL_GET,
	HIR_LOCAL_SET,
	HIR_LOCAL_REF,
	HIR_SYM_GET,
	HIR_SYM_SET,
	HIR_INTEGER_LITERAL,
	HIR_COPY,
	HIR_PHI,
	HIR_CALL,
	HIR_JMP,
	HIR_RETURN,
	HIR_PREFIX,
	HIR_INFIX,
};

struct hir_inst_t {
	hir_inst_kind_t kind;
	hir_rinst_t id;
	loc_t loc;
	type_t type; // -1 for none, TYPE_UNKNOWN is something else
	
	union {
		struct {
			u16 local;
		} d_local;
		struct {
			u16 local;
			hir_rinst_t src;
		} d_local_set;
		struct {
			tok_t op;
			hir_rinst_t val;
		} d_prefix;
		struct {
			tok_t op;
			hir_rinst_t rhs;
			hir_rinst_t lhs;
		} d_infix;
		struct {
			istr_t lit;
		} d_sym;
		struct {
			istr_t lit;
			hir_rinst_t src;
		} d_sym_set;
		struct {
			istr_t lit;
			bool negate;
		} d_literal;
		struct {
			hir_rinst_t *retl;
			u8 retc;
		} d_return;
		/* struct {
		} d_phi; */
	};
};

typedef struct hir_inst_t hir_inst_t;
typedef struct hir_proc_t hir_proc_t;
typedef enum hir_inst_kind_t hir_inst_kind_t;

void dump_proc(hir_proc_t *proc);