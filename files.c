#include "all.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

// it's okay to leak a little on err conditions...
const char* __asan_default_options(void) { return "detect_leaks=0"; }

size_t file_entry_count;
file_entry_t file_entries[256];

// will read entire file into memory
bool file_slurp(FILE* file, const char *fp, rfile_t *handle) {
	assert(file_entry_count < ARRAYLEN(file_entries));

	if (file == NULL) {
		return false;
	}

	bool ret = false;
	int old_errno;
	rfile_t f = file_entry_count;

	file_entry_t *entry = &file_entries[file_entry_count++];
	entry->fp = fp;

	// TODO: impl stdin
	// if (isatty(fileno(file))) {}

	// read file into memory
	if (fseek(file, 0, SEEK_END)) {
		goto cleanup;
	}
	long len = ftell(file);
	if (len == -1) {
		goto cleanup;
	}
	entry->len = len;
	if (fseek(file, 0, SEEK_SET)) {
		goto cleanup;
	}
	entry->data = malloc(entry->len);
	if (fread(entry->data, 1, entry->len, file) < entry->len) {
		goto cleanup;
	}

	ret = true;
	*handle = f;
	
cleanup:
	// TODO: read up on this, test this, does this even do what i want?
	old_errno = errno;
	fclose(file);
	errno = old_errno;
	return ret;
}