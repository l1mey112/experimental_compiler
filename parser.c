#include "all.h"

#include <ctype.h>
#include <assert.h>

typedef struct parse_ctx_t parse_ctx_t;

struct parse_ctx_t {
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;

};

const char *tok_str(tok_t tok) {
	if (0);
    #define X(val, lit) \
		else if(val == tok) return lit;
    TOK_X_LIST
    #undef X

	assert_not_reached();
}

parse_ctx_t parse_ctx;

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

static token_t parser_next() {
	
}

void file_parse(rfile_t file) {
	parse_ctx = (parse_ctx_t){
		
	};
}
