#include "all.h"
#include <assert.h>
#include <errno.h>
#include <string.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

int main(int argc, const char *argv[]) {
	for (int i = 1; i < argc; i++) {
		const char *p = argv[i];
		
		if (p[0] == '-') {
			assert(0 && "TODO: flags not implemented yet");
		}

		rfile_t handle;

		FILE *f = fopen(p, "r");

		if (!file_slurp(f, p, &handle)) {
			eprintf("error: failed to read file '%s' - %s\n", p, strerror(errno));
			return 1;
		}

		// TODO: parse file
	}

	return 0;
}