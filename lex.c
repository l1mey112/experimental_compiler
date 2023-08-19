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

hlex_token_t hlex_next(hlex_t *lex, hcc_ctx_t *ctx) {
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

			htok_t type = htok_ident;

			if (hsv_memcmp_literal(start, len, "fn")) {
				type = htok_fn;
			} else if (hsv_memcmp_literal(start, len, "extern")) {
				type = htok_extern;
			} else if (hsv_memcmp_literal(start, len, "asm")) {
				type = htok_asm;
			}

			return (hlex_token_t){
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

			return (hlex_token_t){
				.type = htok_integer,
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

			hlex_token_t token = {
				.p = lex->pc,
				.row = row,
				.col = col,
			};
			
			switch (ch) {
				case '+':
					if (ch1 == '+') {
						tok = htok_inc;
						lex->pc++;
						len = 2;
					} else {
						tok = htok_add;
					}
					break;
				case '-':
					if (ch1 == '-') {
						tok = htok_dec;
						lex->pc++;
						len = 2;
					} else {
						tok = htok_sub;
					}
					break;
				case '*':
					tok = htok_mul;
					break;
				case '/':
					tok = htok_div;
					break;
				case '%':
					tok = htok_mod;
					break;
				case '=':
					if (ch1 == '=') {
						tok = htok_eq;
						lex->pc++;
						len = 2;
					} else {
						tok = htok_assign;
					}
					break;
				case '!':
					if (ch1 == '=') {
						tok = htok_neq;
						lex->pc++;
					} else {
						tok = htok_not;
					}
					break;
				case '<':
					if (ch1 == '<') {
						tok = htok_lshift;
						len = 2;
						lex->pc++;
					} else {
						tok = htok_lt;
					}
					break;
				case '>':
					if (ch1 == '>') {
						if (ch2 == '>') {
							tok = htok_rushift;
							len = 3;
							lex->pc += 2;
						} else {
							len = 2;
							tok = htok_rshift;
							lex->pc++;
						}
					} else {
						tok = htok_gt;
					}
					break;
				case '&':
					if (ch1 == '&') {
						tok = htok_and;
						len = 2;
						lex->pc++;
					} else {
						tok = htok_band;
					}
					tok = htok_band;
					break;
				case '|':
					if (ch1 == '|') {
						tok = htok_or;
						len = 2;
						lex->pc++;
					} else {
						tok = htok_bor;
					}
					break;
				case '^':
					tok = htok_xor;
					break;
				case '~':
					tok = htok_tilde;
					break;
				case '.':
					tok = htok_dot;
					break;
				case ',':
					tok = htok_comma;
					break;
				case '(':
					tok = htok_opar;
					break;
				case ')':
					tok = htok_cpar;
					break;
				case '[':
					tok = htok_osq;
					break;
				case ']':
					tok = htok_csq;
					break;
				case '{':
					tok = htok_obrace;
					break;
				case '}':
					tok = htok_cbrace;
					break;
				case ':':
					tok = htok_colon;
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

void hlex_token_dump(hlex_token_t token) {
	printf("%u:%u:\t", token.row, token.col);
	printf("'%.*s' - %s\n", token.len, token.p, htok_name(token.type));
}