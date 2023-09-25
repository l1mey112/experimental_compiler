#include "all.h"

#include <ctype.h>
#include <assert.h>
#include <stdio.h>

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

token_t parser_lex_next(void) {
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

			// TODO: this should be optimised to a static hash table

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

			// TODO: ~~this should be optimised to a static hash table~~
			//       maybe not, it's not that much faster

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

void parser_next(void) {
	parser_ctx.tok = parser_ctx.peek;
	parser_ctx.peek = parser_lex_next();
}

#define DEFAULT_DBG_TOK(expected) (token_t){.type = expected, .lit = (istr_t)-1}

void parser_check(tok_t expected) {
	if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else if (parser_ctx.tok.type != expected) {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

void parser_expect(tok_t expected) {
	if (parser_ctx.tok.type == expected) {
		parser_next();
	} else if (parser_ctx.tok.type == TOK_EOF) {
		err_with_pos(parser_ctx.tok.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else {
		err_with_pos(parser_ctx.tok.loc, "unexpected %s, expected %s", tok_dbg_str(parser_ctx.tok), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

#undef DEFAULT_DBG_TOK

void NORETURN parser_unexpected(const char *err) {
	err_with_pos(parser_ctx.tok.loc, "unexpected %s, %s", tok_dbg_str(parser_ctx.tok), err);
}
