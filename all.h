#pragma once

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include <stdbool.h>
#include <assert.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

#ifdef NDEBUG
	#define assert_not_reached()  __builtin_unreachable()
#else
	#define assert_not_reached()  assert(0 && "assert_not_reached")
#endif

#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define MAYBE_UNUSED __attribute__((unused))
#define ARRAYLEN(v) ((u32)(sizeof(v) / sizeof(*(v))))

typedef u32 rstr_t;
typedef u16 rfile_t;
typedef struct loc_t loc_t;

rstr_t intern_sv(u8 *sv, size_t len);
const char *intern_from(rstr_t str);

bool file_slurp(FILE* file, const char *fp, rfile_t *handle);

struct loc_t {
	u32 line_nr;
	u32 col;
	u32 pos;
	u16 len;
	rfile_t file;
};