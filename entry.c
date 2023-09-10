#include "all.h"
#include <alloca.h>
#include <string.h>
#include <setjmp.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

const char* __asan_default_options(void) { return "detect_leaks=0"; }

err_diag_t err_diag;

int main(int argc, const char *argv[]) {
	if (argc != 2) {
		eprintf("usage: %s <file|dir>\n", argv[0]);
		return 1;
	}

	int retval;

	if (setjmp(err_diag.unwind)) {
		// TODO: report error at proper area and print offending line
		//       err_string is good enough for now
		puts(err_diag.err_string);
		retval = 1;
		goto done;
	}

	const char *argv2 = argv[1];

	fs_rnode_t main;
	if (is_our_ext(argv2)) {
		const char *bp = base_path(argv2);
		main = fs_register_root(bp, false);
		fs_slurp_file(argv2, main);
	} else {
		main = fs_register_root(argv2, true);
	}

	// used for quick concrete type lookups
	{
		u32 i = 0;
		#define X(_, lit) typeinfo_concrete_istr[i++] = sv_intern((u8*)lit, strlen(lit));
		TYPE_X_CONCRETE_LITERALS_LIST
		#undef X
		typeinfo_concrete_istr_size = i;
	}

	// queue can grow
	for (fs_rfile_t i = 0; i < fs_files_queue_len; i++) {
		u32 old_sz = fs_files_queue_len;
		eprintf("parsing file '%s'\n", fs_files_queue[i].fp);
		parser_parse_file(i);
		if (old_sz != fs_files_queue_len) {
			eprintf("  %u new files added\n", fs_files_queue_len - old_sz);
		}
	}
	retval = 0;
done:

	fs_dump_tree();

	return retval;
}