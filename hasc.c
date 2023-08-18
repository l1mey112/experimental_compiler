#include <stdio.h>

#include "lex.h"

int main(void) {
	hlex_t lex;
	hlex_token_t tok;

	hlex_init(&lex, (u8 *)"hello world3 223", sizeof("hello world3 223") - 1);

	if (setjmp(lex.lex_err) == 0) {
		while (!hlex_is_eof(&lex)) {
			tok = hlex_next(&lex);
			printf("tok: %d, col: %u, row: %u\n", tok.type, tok.col, tok.row);
		}
		
		printf("passed!\n");
	} else {
		printf("nope!\n");
	}
}