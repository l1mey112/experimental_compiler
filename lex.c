#include "hasc.h"
#include "lex.h"

#include <ctype.h>
#include <stdio.h>
#include <assert.h>

void hlex_init(hlex_t *lex, u8 *pc, size_t plen) {
	*lex = (hlex_t){
		.pc = pc,
		.pend = pc + plen,
		.plast_nl = pc,
	};
}

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

bool hlex_is_eof(hlex_t *lex) {
	return lex->pc >= lex->pend;
}

htoken_t hlex_next(hlex_t *lex, hcc_ctx_t *ctx) {
	while (lex->pc < lex->pend) {
		u8 ch = *lex->pc;

		if (isspace(ch)) {
			lex->pc++;
			if (ch == '\n') {
				lex->plast_nl = lex->pc;
				lex->line_nr++;
			}
			continue;
		}

		if (is_id_begin(ch)) {
			u8 *start = lex->pc;
			do {
				lex->pc++;
			} while (lex->pc < lex->pend && is_id(*lex->pc));

			// get length and id pointer
			size_t len = lex->pc - start;

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
				.row = lex->line_nr,
				.col = lex->pc - lex->plast_nl - len,
				.len = len,
				.p = start,
			};
		} else if (isdigit(ch)) {
			u8 *start = lex->pc;
			do {
				lex->pc++;
			} while (lex->pc < lex->pend && isdigit(*lex->pc));

			// get length and id pointer
			size_t len = lex->pc - start;

			return (htoken_t){
				.type = HTOK_INTEGER,
				.row = lex->line_nr,
				.col = lex->pc - lex->plast_nl - len,
				.len = len,
				.p = start,
			};
		} else {
			htok_t tok;

			u8 ch1 = lex->pc + 1 < lex->pend ? lex->pc[1] : 0;
			u8 ch2 = lex->pc + 2 < lex->pend ? lex->pc[2] : 0;

			u32 row = lex->line_nr;
			u32 col = lex->pc - lex->plast_nl - 1;
			u32 len = 1;

			htoken_t token = {
				.p = lex->pc,
				.row = row,
				.col = col,
			};
			
			switch (ch) {
				case '+':
					if (ch1 == '+') {
						tok = HTOK_INC;
						lex->pc++;
						len = 2;
					} else {
						tok = HTOK_ADD;
					}
					break;
				case '-':
					if (ch1 == '-') {
						tok = HTOK_DEC;
						lex->pc++;
						len = 2;
					} else {
						tok = HTOK_SUB;
					}
					break;
				case '*':
					tok = HTOK_MUL;
					break;
				case '/':
					tok = HTOK_DIV;
					break;
				case '%':
					tok = HTOK_MOD;
					break;
				case '=':
					if (ch1 == '=') {
						tok = HTOK_EQ;
						lex->pc++;
						len = 2;
					} else {
						tok = HTOK_ASSIGN;
					}
					break;
				case '!':
					if (ch1 == '=') {
						tok = HTOK_NEQ;
						lex->pc++;
					} else {
						tok = HTOK_NOT;
					}
					break;
				case '<':
					if (ch1 == '<') {
						tok = HTOK_LSHIFT;
						len = 2;
						lex->pc++;
					} else {
						tok = HTOK_LT;
					}
					break;
				case '>':
					if (ch1 == '>') {
						if (ch2 == '>') {
							tok = HTOK_RUSHIFT;
							len = 3;
							lex->pc += 2;
						} else {
							len = 2;
							tok = HTOK_RSHIFT;
							lex->pc++;
						}
					} else {
						tok = HTOK_GT;
					}
					break;
				case '&':
					if (ch1 == '&') {
						tok = HTOK_AND;
						len = 2;
						lex->pc++;
					} else {
						tok = HTOK_BAND;
					}
					tok = HTOK_BAND;
					break;
				case '|':
					if (ch1 == '|') {
						tok = HTOK_OR;
						len = 2;
						lex->pc++;
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
			lex->pc++;

			return token;
		}
	}

	assert_not_reached(); // can't call next() whilst EOF
}

const char *htok_name(htok_t tok) {
	#define X(name, lit) \
		if (tok == name) return lit;
    #include "tok.def"
    #undef X
	
	return "unknown token";
}

void hlex_token_dump(htoken_t token) {
	printf("%u:%u:\t", token.row, token.col);
	printf("'%.*s' - %s\n", token.len, token.p, htok_name(token.type));
}