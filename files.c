#include "all.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>

typedef struct file_entry_t file_entry_t;

struct file_entry_t {
	const char *fp;
	u8 *data;
	size_t len;
};

static struct {
	size_t entry_count;
	file_entry_t entries[256];
} files;

// will read entire file into memory
bool file_slurp(FILE* file, const char *fp, rfile_t *handle) {
	assert(files.entry_count < ARRAYLEN(files.entries));

	if (file == NULL) {
		return false;
	}

	bool ret = false;
	int old_errno;
	rfile_t f = files.entry_count;

	file_entry_t *entry = &files.entries[files.entry_count++];
	entry->fp = fp;

	// read file into memory
	if (fseek(file, 0, SEEK_END)) {
		goto cleanup;
	}
	if ((entry->len = ftell(file)) == -1) {
		goto cleanup;
	}
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