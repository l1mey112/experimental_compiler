#include "all.h"

#include <ctype.h>
#include <assert.h>
#include <stdio.h>

typedef struct parser_ctx_t parser_ctx_t;

struct parser_ctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	rfile_t file;
	token_t tok;
	token_t peek;
};

// allocates using alloc_* functions
const char *tok_dbg_str(token_t tok) {
	// handle identifiers

	u8 *p;

	bool requires_quotes = true;
	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .type = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	// so you can do something like: "unexpected `x`, expected `+=`"

	if (TOK_HAS_LIT(tok.type) && tok.lit == (rstr_t)-1) {
		requires_quotes = false;
	}
	
	if (TOK_HAS_LIT(tok.type) && tok.lit != (rstr_t)-1) {
		str = sv_from(tok.lit);
		len = strlen(str);
	}
    #define X(val, lit) \
		else if (val == tok.type) str = lit, len = strlen(lit);
    TOK_X_LIST
    #undef X

	if (requires_quotes) {
		p = alloc_scratch(len + 2 + 1);
		sprintf((char *)p, "`%s`", str);
	} else {
		p = alloc_scratch(len + 1);
		strcpy((char *)p, str);
	}

	return (const char *)p;
}

static parser_ctx_t parser_ctx;

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

static token_t parser_lex_next(void) {
	while (parser_ctx.pc < parser_ctx.pend) {
		u8 ch = *parser_ctx.pc;

		if (isspace(ch)) {
			parser_ctx.pc++;
			if (ch == '\n') {
				parser_ctx.plast_nl = parser_ctx.pc;
				parser_ctx.line_nr++;
			}
			continue;
		}

		if (is_id_begin(ch)) {
			u8 *start = parser_ctx.pc;

			token_t token = {
				.type = TOK_INTEGER,
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};
			
			do {
				parser_ctx.pc++;
			} while (parser_ctx.pc < parser_ctx.pend && is_id(*parser_ctx.pc));

			// get length and id pointer
			u32 len = parser_ctx.pc - start;
			token.loc.len = len;

			if (0);
			#define X(val, lit) \
				else if (sv_cmp_literal(start, len, lit)) token.type = val;
			TOK_X_KEYWORDS_LIST
			#undef X
			else {
				token.type = TOK_IDENT;
				token.lit = sv_intern(start, len);
			}

			return token;
		} else if (isdigit(ch)) {
			u8 *start = parser_ctx.pc;

			token_t token = {
				.type = TOK_INTEGER,
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};
			
			do {
				parser_ctx.pc++;
			} while (parser_ctx.pc < parser_ctx.pend && isdigit(*parser_ctx.pc));

			// get length and id pointer
			u32 len = parser_ctx.pc - start;

			token.lit = sv_intern(start, len);
			token.loc.len = len;

			return token;
		} else {
			u8 *start = parser_ctx.pc;
			size_t avail = parser_ctx.pend - parser_ctx.pc;

			// the compiler optimiser would be able to optimise and
			// spot locations of compile time known bounds to memcmp
			// ... where it can
			//
			// this isn't perfect, the old switch case impl would be better
			// the macro method reduces complexity on implementation
			// i know well that the compiler will NOT optimise this efficiently

			token_t token = {
				.loc.line_nr = parser_ctx.line_nr,
				.loc.col = parser_ctx.pc - parser_ctx.plast_nl,
				.loc.file = parser_ctx.file,
				.loc.pos = start - parser_ctx.pstart,
			};

			if (0);
			#define X(val, lit) \
				else if (strlen(lit) <= avail && memcmp(start, lit, strlen(lit)) == 0) { \
					token.type = val; \
					token.loc.len = strlen(lit); \
					parser_ctx.pc += strlen(lit); \
				}
			TOK_X_OPERATOR_LIST
			#undef X
			else {
				err_with_pos(token.loc, "unexpected character `%c`", ch);
			}

			return token;
		}
	}

	return (token_t){.type = TOK_EOF};
}

static void parser_next(void) {
	parser_ctx.tok = parser_ctx.peek;
	parser_ctx.peek = parser_lex_next();
}

#define DEFAULT_DBG_TOK(expected) (token_t){.type = expected, .lit = (rstr_t)-1}

static void parser_check(tok_t expected) {
	if (parser_ctx.tok.type != expected) {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

static void parser_expect(tok_t expected) {
	if (parser_ctx.tok.type == expected) {
		parser_next();
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

static void parser_expect_not_eof() {
	if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF");
	} else {
		parser_next();
	}
}

#undef DEFAULT_DBG_TOK

void parser_top_stmt() {
	
}

void file_parse(rfile_t file) {
	file_entry_t *f = &file_entries[file];
	
	parser_ctx = (parser_ctx_t){
		.file = file,
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
	};

	parser_next(); // tok
	parser_next(); // tok peek

	parser_check(TOK_IDENT);

	while (parser_ctx.tok.type != TOK_EOF) {
		parser_top_stmt();
	}
}