#pragma once

#include <stdint.h>
#include <sys/types.h>
#include <stdbool.h>
#include <assert.h>
#include <stdnoreturn.h>
#include <string.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef const char *owned_string;

#ifdef NDEBUG
	#define assert_not_reached()  __builtin_unreachable()
#else
	#define assert_not_reached()  assert(0 && "assert_not_reached")
#endif

#define MAYBE_UNUSED __attribute__((unused))
#define ARRAYSIZE(_ARR) ((int)(sizeof(_ARR) / sizeof(*(_ARR)))) // Size of a static C-style array. Don't use on pointers!

#define hsv_memcmp_literal(a, alen, b) hsv_memcmp(a, alen, (u8 *)b, sizeof(b "") - 1)

static MAYBE_UNUSED u8 *memdup(u8 *ptr, size_t size) {
	void *out = malloc(size);

   	if (out != NULL)
		memcpy(out, ptr, size);

	return out;
}

static MAYBE_UNUSED bool hsv_memcmp(u8 *a, size_t alen, u8 *b, size_t blen) {
    if (alen != blen) {
        return false;
    }
    return memcmp(a, b, alen) == 0;
}

static MAYBE_UNUSED owned_string hsv_make_owned(u8 *a, size_t alen) {
	char *p;

	assert((p = malloc(alen + 1)));

	memcpy(p, a, alen);
	p[alen] = 0;

	return p;
}