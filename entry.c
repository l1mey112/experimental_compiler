#include "all.h"
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <setjmp.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

err_diag_t err_diag;

int main(int argc, const char *argv[]) {
	for (int i = 1; i < argc; i++) {
		const char *p = argv[i];
		FILE *f = NULL;
		
		if (p[0] == '-' && p[0] == '\0') {
			// FILE *f = stdin;
			assert(0 && "TODO: not implement yet"); // cannot use fseek on 0 fd
		} else if (p[0] == '-') {
			assert(0 && "TODO: flags not implemented yet");
		} else {
			f = fopen(p, "r");
		}

		rfile_t handle;

		if (!file_slurp(f, p, &handle)) {
			eprintf("error: failed to read file '%s' - %s\n", p, strerror(errno));
			return 1;
		}

		(void)handle;
	}

	if (!setjmp(err_diag.unwind)) {
		for (rfile_t i = 0; i < file_entry_count; i++) {
			file_parse(i);
		}
	} else {
		// TODO: report error at proper area and print offending line
		//       err_string is good enough for now
		puts(err_diag.err_string);
		return 1;
	}

	return 0;
}