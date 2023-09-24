#pragma once

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
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
#define eputs(v) fputs(v, stderr); fputc('\n', stderr)

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

// util
typedef u32 istr_t;
typedef struct err_diag_t err_diag_t;
// types
typedef u16 type_t;
typedef struct typeinfo_t typeinfo_t;
typedef enum typeinfo_kind_t typeinfo_kind_t;
// symbols
typedef u32 rsym_t;
typedef struct sym_t sym_t;
typedef enum sym_kind_t sym_kind_t;
// token
typedef struct token_t token_t;
typedef struct loc_t loc_t;
typedef enum tok_t tok_t;
// PIR
typedef struct pir_proc_t pir_proc_t;
typedef struct pir_block_t pir_block_t;
typedef struct pir_local_t pir_local_t;
typedef struct pir_inst_t pir_inst_t;
typedef enum pir_inst_kind_t pir_inst_kind_t;
typedef struct sym_resolve_t sym_resolve_t;
typedef u32 pir_rlocal_t;
typedef u32 pir_rinst_t;
typedef u32 pir_rblock_t;
// module fs
typedef u32 fs_rfile_t;
typedef u32 fs_rnode_t;
typedef struct fs_node_t fs_node_t;
typedef struct fs_file_t fs_file_t;

#define TYPE_UNRESOLVED ((type_t)-1)
#define SYM_UNRESOLVED ((rsym_t)-1)

const char *last_path(const char* path);
const char *base_path(const char* path);
const char *relative_path_of_exe(void);

bool is_our_ext(const char *fp);
void fs_slurp_file(const char *p, fs_rnode_t mod);
void fs_slurp_dir(fs_rnode_t ref);
fs_rnode_t fs_register_root(const char *p, bool is_main, bool slurp);
fs_rnode_t fs_register_import(fs_rnode_t src, fs_rnode_t *path, u32 path_len, loc_t loc);
fs_file_t *fs_filep(fs_rfile_t ref);
fs_node_t *fs_nodep(fs_rnode_t ref);
const char *fs_module_symbol_sv(fs_rnode_t module, istr_t symbol);
istr_t fs_module_symbol_str(fs_rnode_t module, istr_t symbol);
void fs_dump_tree(void);

istr_t sv_intern(u8 *sv, size_t len);
istr_t sv_move(const char *p);
const char *sv_from(istr_t str);
ptrdiff_t sv_index(const char *p);

void parser_parse_file(fs_rfile_t file);

// node in a directory tree, also a module
// root fs_nodes don't have a name, except when is_main
struct fs_node_t {
	const char *path;
	fs_rnode_t parent;
	fs_rnode_t *children;
	u32 children_len;
	u32 our_files;
	istr_t name; // shortname
	bool is_src_scanned; // importing a module where == true and our_files == 0 is an error
	bool is_main;
};

struct fs_file_t {
	const char *fp;
	u8 *data;
	size_t len;
	fs_rnode_t module;
};

extern u32 fs_files_queue_len;
extern fs_file_t fs_files_queue[512];

// TODO: make smaller?
struct loc_t {
	u32 line_nr;
	u32 col;
	u32 pos;
	u16 len;
	fs_rfile_t file;
};

struct err_diag_t {
	jmp_buf unwind;
	char err_string[256];
	// TODO: more err information
	// loc_t err_loc;
};

extern err_diag_t err_diag;

void err_with_pos(loc_t loc, const char *fmt, ...)
	__attribute__((format(printf, 2, 3))) __attribute__ ((__noreturn__));

void err_without_pos(const char *fmt, ...)
	__attribute__((format(printf, 1, 2))) __attribute__ ((__noreturn__));

#define TOK_X_KEYWORDS_LIST \
	X(TOK_FN, "fn") \
	X(TOK_ASM, "asm") \
	X(TOK_AS, "as") \
	X(TOK_RETURN, "return") \
	X(TOK_MUT, "mut") \
	X(TOK_IF, "if") \
	X(TOK_ELSE, "else") \
	X(TOK_FOR, "for") \
	X(TOK_BREAK, "break") \
	X(TOK_CONTINUE, "continue") \
	X(TOK_IMPORT, "import") \
	X(TOK_PUB, "pub")

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

type_t type_new(typeinfo_t typeinfo);
typeinfo_t *type_get(type_t type);
type_t type_new_inc_mul(type_t type);
const char *type_dbg_str(type_t type);

// TODO: discrepancy
//       function arguments aren't locals in the normal sense, they're immutable value insts
//       their address (COULD?) be taken??
//       okay, fuck! their address is taken.
//       variables should be declared using new var to take part in liveness too

struct pir_local_t {
	istr_t name;
	loc_t name_loc;
	type_t type;
	loc_t type_loc;
	bool is_arg;
	bool is_mut;
};

struct pir_proc_t {
	istr_t name;
	loc_t name_loc;
	type_t type;
	u16 args;
	u16 rets;
	pir_rblock_t entry;
	pir_local_t *locals;
	pir_block_t *blocks;
	pir_inst_t *insts;
};

struct pir_block_t {
	pir_rblock_t id;
	pir_rinst_t first;
	pir_rinst_t last;
	u32 len;
};

// TODO: PIR_TMP_LOCAL ?? (probably just allow compiler defined PIR_LOCALs)
//
// PIR_ARG   : function arguments.
// PIR_SYM   : unresolved symbol. checker will resolve
// PIR_LOCAL : resolved symbol in parsing phase, local.
//
enum pir_inst_kind_t {
	PIR_NOP,
	// PIR_ARG,
	// PIR_LOCAL,
	// PIR_SYM,
	PIR_LLOAD,
	PIR_LSTORE,
	PIR_INTEGER_LITERAL,
	PIR_ADDR_OF,
	PIR_CALL,
	PIR_RETURN,
	PIR_PREFIX,
	PIR_INFIX,
	// PIR_FIELD
	PIR_JMP,
	PIR_IF,
};

struct sym_resolve_t {
	rsym_t sym;
	struct unresolved_t {
		fs_rnode_t module;
		istr_t lit;
	} d_unresolved;
};

struct pir_inst_t {
	pir_inst_kind_t kind;
	pir_rinst_t id;
	pir_rinst_t next;
	pir_rinst_t prev;
	loc_t loc;
	type_t type; // -1 for none, TYPE_UNKNOWN is something else
	
	union {
		// PIR_ADDR_OF
		struct {
			pir_rinst_t src;
			bool is_mut_ref;
		} d_addr_of;
		// PIR_LSTORE
		struct {
			union {
				pir_rlocal_t local;
				sym_resolve_t sym;
			};
			pir_rinst_t src;
			bool is_sym;
		} d_store;
		// PIR_LLOAD
		struct {
			union {
				pir_rlocal_t local;
				sym_resolve_t sym;
			};
			bool is_sym;
		} d_load;
		// PIR_PREFIX
		struct {
			tok_t op;
			pir_rinst_t val;
		} d_prefix;
		// PIR_CALL
		struct {
			tok_t op;
			pir_rinst_t rhs;
			pir_rinst_t lhs;
		} d_infix;
		// PIR_INTEGER_LITERAL
		struct {
			istr_t lit;
			bool negate;
		} d_literal;
		// PIR_RETURN
		struct {
			pir_rinst_t *ilist;
			u16 ilen;
		} d_return;
		// PIR_CALL
		struct {
			pir_rinst_t target;
			pir_rinst_t *ilist;
			u16 ilen;
		} d_call;
		// PIR_JMP
		pir_rblock_t d_jmp;
		// PIR_IF
		struct {
			pir_rinst_t cond;
			pir_rblock_t on_true;
			pir_rblock_t on_false;
		} d_if;
	};
};

pir_inst_t *pir_at(pir_proc_t *proc, pir_rinst_t inst);
pir_rinst_t pir_insert_after(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at, pir_inst_t inst);
pir_rinst_t pir_insert_before(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at, pir_inst_t inst);
pir_rinst_t pir_insert(pir_proc_t *proc, pir_rblock_t bb_ref, pir_inst_t inst);
void pir_delete(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at);
pir_inst_t pir_pop(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at);
pir_rblock_t pir_new_block(pir_proc_t *proc);

enum sym_kind_t {
	SYM_GLOBAL,
	SYM_CONST,
	SYM_PROC,
};

struct sym_t {
	istr_t key;
	sym_kind_t kind;
	type_t type;
	fs_rnode_t module;
	loc_t name_loc;
	bool is_pub;
	union {
		pir_proc_t proc;
	};
};

extern sym_t *table;
rsym_t table_new(sym_t sym);
rsym_t table_retrieve_field(fs_rnode_t mod, istr_t lit);
bool table_resolve(sym_resolve_t *resolve, fs_rnode_t src_module, loc_t loc);
void table_dump(bool list_ir);

// TODO: move to table.c?
void dump_proc(pir_proc_t *proc);

void check_all(void);