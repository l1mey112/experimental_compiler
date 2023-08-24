#include <stdio.h>

#include "tok.h"

const char *htok_name(htok_t tok) {
	#define X(name, lit) \
		if (tok == name) return lit;
    #include "tok.def"
    #undef X
	
	return "unknown token";
}

void htoken_dump(htoken_t token) {
	printf("%u:%u:\t", token.row, token.col);
	printf("'%.*s' - %s\n", token.len, token.p, htok_name(token.type));
}
