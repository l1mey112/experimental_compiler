#include <stdio.h>

#include "tok.h"

const char *htok_name(htok_t tok) {
	#define X(name, lit) \
		if (tok == name) return lit;
    #include "tok.def"
    #undef X
	
	return "unknown token";
}

// use ctx to locate file entry
void htoken_dump(htoken_t token) {
	assert_not_reached();
	// printf("%u:%u:\t", token.loc.line_nr, token.loc.col);
	// printf("'%.*s' - %s\n", token.loc.len, token.loc.p, htok_name(token.type));
}
