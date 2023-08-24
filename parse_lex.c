#include "hasc.h"
#include "tok.h"

#include <ctype.h>
#include <stdio.h>
#include <assert.h>

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

htoken_t hparser_lex_next(hparser_t *parser, hcc_ctx_t *ctx) {
	while (parser->lex.pc < parser->lex.pend) {
		u8 ch = *parser->lex.pc;

		if (isspace(ch)) {
			parser->lex.pc++;
			if (ch == '\n') {
				parser->lex.plast_nl = parser->lex.pc;
				parser->lex.line_nr++;
			}
			continue;
		}

		if (is_id_begin(ch)) {
			u8 *start = parser->lex.pc;
			do {
				parser->lex.pc++;
			} while (parser->lex.pc < parser->lex.pend && is_id(*parser->lex.pc));

			// get length and id pointer
			size_t len = parser->lex.pc - start;

			// TODO: keywords, token types...

			htok_t type = HTOK_IDENT;

			if (0);
			#define X_ONLY_LITERALS
			#define X(name, lit) \
				else if (hsv_memcmp_literal(start, len, lit)) type = name;
			#include "tok.def"
			#undef X
			#undef X_ONLY_LITERALS

			return (htoken_t){
				.type = type,
				.row = parser->lex.line_nr,
				.col = parser->lex.pc - parser->lex.plast_nl - len,
				.len = len,
				.p = start,
			};
		} else if (isdigit(ch)) {
			u8 *start = parser->lex.pc;
			do {
				parser->lex.pc++;
			} while (parser->lex.pc < parser->lex.pend && isdigit(*parser->lex.pc));

			// get length and id pointer
			size_t len = parser->lex.pc - start;

			return (htoken_t){
				.type = HTOK_INTEGER,
				.row = parser->lex.line_nr,
				.col = parser->lex.pc - parser->lex.plast_nl - len,
				.len = len,
				.p = start,
			};
		} else {
			htok_t tok;

			u8 ch1 = parser->lex.pc + 1 < parser->lex.pend ? parser->lex.pc[1] : 0;
			u8 ch2 = parser->lex.pc + 2 < parser->lex.pend ? parser->lex.pc[2] : 0;

			u32 row = parser->lex.line_nr;
			u32 col = parser->lex.pc - parser->lex.plast_nl - 1;
			u32 len = 1;

			htoken_t token = {
				.p = parser->lex.pc,
				.row = row,
				.col = col,
			};
			
			switch (ch) {
				case '+':
					if (ch1 == '+') {
						tok = HTOK_INC;
						parser->lex.pc++;
						len = 2;
					} else if (ch1 == '=') {
						tok = HTOK_ASSIGN_ADD;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_ADD;
					}
					break;
				case '-':
					if (ch1 == '-') {
						tok = HTOK_DEC;
						parser->lex.pc++;
						len = 2;
					} else if (ch1 == '=') {
						tok = HTOK_ASSIGN_SUB;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_SUB;
					}
					break;
				case '*':
					if (ch1 == '=') {
						tok = HTOK_ASSIGN_MUL;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_MUL;
					}
					break;
				case '/':
					if (ch1 == '=') {
						tok = HTOK_ASSIGN_DIV;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_DIV;
					}
					break;
				case '%':
					if (ch1 == '=') {
						tok = HTOK_ASSIGN_MOD;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_MOD;
					}
					break;
				case '=':
					if (ch1 == '=') {
						tok = HTOK_EQ;
						parser->lex.pc++;
						len = 2;
					} else {
						tok = HTOK_ASSIGN;
					}
					break;
				case '!':
					if (ch1 == '=') {
						tok = HTOK_NEQ;
						parser->lex.pc++;
					} else {
						tok = HTOK_NOT;
					}
					break;
				case '<':
					if (ch1 == '<') {
						tok = HTOK_LSHIFT;
						len = 2;
						parser->lex.pc++;
					} else if (ch1 == '=') {
						tok = HTOK_LE;
						len = 2;
						parser->lex.pc++;
					} else {
						tok = HTOK_LT;
					}
					break;
				case '>':
					if (ch1 == '>') {
						if (ch2 == '>') {
							tok = HTOK_RUSHIFT;
							len = 3;
							parser->lex.pc += 2;
						} else {
							tok = HTOK_RSHIFT;
							len = 2;
							parser->lex.pc++;
						}
					} else if (ch1 == '=') {
						tok = HTOK_GE;
						len = 2;
						parser->lex.pc++;
					} else {
						tok = HTOK_GT;
					}
					break;
				case '&':
					if (ch1 == '&') {
						tok = HTOK_AND;
						len = 2;
						parser->lex.pc++;
					} else {
						tok = HTOK_BAND;
					}
					tok = HTOK_BAND;
					break;
				case '|':
					if (ch1 == '|') {
						tok = HTOK_OR;
						len = 2;
						parser->lex.pc++;
					} else {
						tok = HTOK_BOR;
					}
					break;
				case '^':
					tok = HTOK_XOR;
					break;
				case '~':
					tok = HTOK_TILDE;
					break;
				case '.':
					tok = HTOK_DOT;
					break;
				case ',':
					tok = HTOK_COMMA;
					break;
				case '(':
					tok = HTOK_OPAR;
					break;
				case ')':
					tok = HTOK_CPAR;
					break;
				case '[':
					tok = HTOK_OSQ;
					break;
				case ']':
					tok = HTOK_CSQ;
					break;
				case '{':
					tok = HTOK_OBRACE;
					break;
				case '}':
					tok = HTOK_CBRACE;
					break;
				case ':':
					tok = HTOK_COLON;
					break;
				case '?':
					tok = HTOK_QUESTION;
					break;
				default:
					hcc_err_with_pos(ctx, token, "unexpected character '%c'", ch);
			}

			token.len = len;
			token.type = tok;
			parser->lex.pc++;

			return token;
		}
	}

	static htoken_t eof = {.type = HTOK_EOF};

	return eof;
}

