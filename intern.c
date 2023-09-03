#include "all.h"

#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include "stb_ds.h"

typedef struct { char *key; } intern_key_t;
static intern_key_t *interns = NULL;

static char *sv_to_cstring(u8 *str, size_t len) {
	char *result = malloc(len + 1);
	memcpy(result, str, len);
	result[len] = '\0';
	return result;
}

istr_t sv_intern(u8 *sv, size_t len) {
	char *str = sv_to_cstring(sv, len);

	if (interns == NULL) {
		sh_new_arena(interns);
	}

	ptrdiff_t result = shgeti(interns, str);

	if (result == -1) {
		shputs(interns, (intern_key_t) { .key = str });
		result = shlen(interns) - 1;
	}

	assert(result >= 0 && (size_t)result <= UINT32_MAX);

	return (u32)result;
}

const char *sv_from(istr_t str) {
	assert(str >= 0 && str < shlen(interns));
	return interns[str].key;
}
