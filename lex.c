#include "lex.h"
#include <ctype.h>

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

hlex_token_t hlex_next(hlex_t *lex) {
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

			return (hlex_token_t){
				.type = htok_ident,
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
			
			switch (ch) {
				case '+':
					if (ch1 == '+') {
						tok = htok_inc;
						lex->pc++;
					} else {
						tok = htok_add;
					}
					break;
				case '-':
					if (ch1 == '-') {
						tok = htok_dec;
						lex->pc++;
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
					} else {
						tok = htok_assign;
					}
					break;
				case '!':
					if (ch1 == '=') {
						tok = htok_neq;
					} else {
						tok = htok_not;
					}
					break;
				case '<':
					if (ch1 == '<') {
						tok = htok_lshift;
						lex->pc++;
					} else {
						tok = htok_lt;
					}
					break;
				case '>':
					if (ch1 == '>') {
						if (ch2 == '>') {
							tok = htok_rushift;
							lex->pc += 2;
						} else {
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
						lex->pc++;
					} else {
						tok = htok_band;
					}
					tok = htok_band;
					break;
				case '|':
					if (ch1 == '|') {
						tok = htok_or;
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
				default:
					// TODO: set last error
					longjmp(lex->lex_err, 1);
			}

			lex->pc++;

			return (hlex_token_t){
				.type = tok,
				.row = lex->line_nr,
				.col = lex->pc - lex->plast_nl - 1,
			};
		}
	}

	assert_not_reached();
}
