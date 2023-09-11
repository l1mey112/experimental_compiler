#include "all.h"

#include <setjmp.h>
#include <stdarg.h>
#include <linux/limits.h>
#include <stdio.h>
#include <unistd.h>
#include "stb_ds.h"

const char *last_path(const char* path) {
	const char* sp = strrchr(path, '/');

	if (sp == NULL) {
		return path;
	}

	return strdup(sp + 1);
}

const char *base_path(const char* path) {
	const char* sp = strrchr(path, '/');

	if (sp == NULL) {
		return ".";
	}

	size_t len = sp - path;
	char *base = (char *)malloc(len + 1);

	memcpy(base, path, len);
	base[len] = '\0';

	return base;
}

const char *make_relative(const char *cwd, const char *path) {
	if (strlen(cwd) > strlen(path)) {
		return path;
	}
	while (*cwd != '\0' && *cwd == *path) {
		cwd++;
		path++;
	}
	while (*path != '\0' && *path == '/') {
		path++;
	}
	return path;
}

const char *relative_path_of_exe(void) {
	char *scratch = (char *)alloc_scratch(0);
	ssize_t len = readlink("/proc/self/exe", scratch, PATH_MAX);
	if (len < 0) {
		err_without_pos("error: could not read `/proc/self/exe`");
	}
	scratch[len] = '\0';
	const char *exe_path = base_path(scratch); // will dup
	if (!getcwd(scratch, PATH_MAX)) {
		err_without_pos("error: could not get current working directory");
	}
	return strdup(make_relative(scratch, exe_path));
}

void err_with_pos(loc_t loc, const char *fmt, ...) {
	char buf[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	fs_file_t *file = fs_filep(loc.file);

	snprintf(err_diag.err_string, sizeof(err_diag.err_string), "%s:%u:%u: %s", file->fp, loc.line_nr + 1, loc.col + 1, buf);
	longjmp(err_diag.unwind, 1);
}

void err_without_pos(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vsnprintf(err_diag.err_string, sizeof(err_diag.err_string), fmt, args);
	va_end(args);
	longjmp(err_diag.unwind, 1);
}

// used for memory that lasts forever, simple bump ptr allocator
static u8 scratch_buf[16384];
static u8 *scratch_p = scratch_buf;

u8 *alloc_scratch(size_t size) {
	assert(scratch_p + size <= scratch_buf + sizeof(scratch_buf));
	
	u8 *p = scratch_p;
	scratch_p += size;
	return p;
}

void alloc_reset(u8 *p) {
	assert(p >= scratch_buf && p <= scratch_buf + sizeof(scratch_buf));
	scratch_p = p;
}

#define LINE_LEN_PADDING 20

static void _padding_to_size(u32 line_len) {
	if (line_len < LINE_LEN_PADDING) {
		eprintf("%*c", LINE_LEN_PADDING - line_len, ' ');
	}
}

static const char *_inst_str(pir_proc_t *proc, pir_rinst_t inst) {
	char *s = (char *)alloc_scratch(0);

	u32 len = sprintf(s, "%%%u%s", proc->insts[inst].id, proc->insts[inst].is_lvalue ? ":l" : "");

	return (const char *)alloc_scratch(len + 1);
}

// TODO: insert types? if the value is `void` no need to assign
static void _dump_inst(pir_proc_t *proc, pir_inst_t *inst) {
	/* if (inst->type == TYPE_VOID) {
		eprintf("     = ");
	} else {
		eprintf("%%%-3u = ", inst->id);
	} */
	u32 line_len = 0;
	if (inst->kind != PIR_STORE && inst->kind != PIR_RETURN) {
		line_len += eprintf("%s = ", _inst_str(proc, inst->id));
	}
	switch (inst->kind) {
		case PIR_ARG: {
			pir_local_t *local = &proc->locals[inst->d_local];
			line_len += eprintf("arg(%u)", inst->d_local);
			_padding_to_size(line_len);
			eprintf("; def %s: %s\n", sv_from(local->name), type_dbg_str(local->type));
			break;
		}
		case PIR_LOCAL: {
			pir_local_t *local = &proc->locals[inst->d_local];
			line_len += eprintf("local(%u)", inst->d_local);
			_padding_to_size(line_len);
			eprintf("; def %s%s: %s\n", local->is_mut ? "mut " : "", sv_from(local->name), type_dbg_str(local->type));
			break;
		}
		/* case PIR_SYM:
			if (inst->d_sym.resv == pir_INST_RESOLVED_LOCAL) {
				pir_local_t *local = &proc->locals[inst->d_sym.data.local];
				line_len += eprintf("sym(%s)", sv_from(local->name));
				_padding_to_size(line_len);
				if (local->is_arg) {
					eprintf("; arg%u %s: %s\n", local->inst, sv_from(local->name), type_dbg_str(local->type));
				} else {
					eprintf("; %s%s: %s\n", local->is_mut ? "mut " : "", sv_from(local->name), type_dbg_str(local->type));
				}
			} else {
				eprintf("sym(%s)\n", sv_from(inst->d_sym.data.lit));
			}
			break; */
		case PIR_SYM:
			eprintf("sym(%s)\n", sv_from(inst->d_sym.data.lit));
		case PIR_LOAD:
			eprintf("load %s\n", _inst_str(proc, inst->d_load.src));
			break;
		case PIR_STORE:
			eprintf("store %s, %s\n", _inst_str(proc, inst->d_store.dest), _inst_str(proc, inst->d_store.src));
			break;
		case PIR_INTEGER_LITERAL:
			if (inst->d_literal.negate) {
				eprintf("-%s\n", sv_from(inst->d_literal.lit));
			} else {
				eprintf("%s\n", sv_from(inst->d_literal.lit));
			}
			break;
		case PIR_INFIX:
			eprintf("%s %s %s\n", _inst_str(proc, inst->d_infix.lhs), tok_literal_representation(inst->d_infix.op), _inst_str(proc, inst->d_infix.rhs));
			break;
		case PIR_PREFIX:
			eprintf("%s %s\n", tok_literal_representation(inst->d_prefix.op), _inst_str(proc, inst->d_prefix.val));
			break;
		case PIR_CALL:
			eprintf("call %s(", _inst_str(proc, inst->d_call.target));
			for (u32 i = 0; i < inst->d_call.ilen; i++) {
				eprintf("%s", _inst_str(proc, inst->d_call.ilist[i]));
				if (i + 1 < inst->d_call.ilen) {
					eprintf(", ");
				}
			}
			eprintf(")\n");
			break;
		case PIR_RETURN:
			eprintf("return");
			for (u32 i = 0; i < inst->d_return.ilen; i++) {
				eprintf(" %s", _inst_str(proc, inst->d_return.ilist[i]));
				if (i + 1 < inst->d_return.ilen) {
					eprintf(",");
				}
			}
			eprintf("\n");
			break;
		default:
			assert_not_reached();
	}
}

void dump_proc(pir_proc_t *proc) {
	u8 *sc = alloc_scratch(0);

	eprintf("%s: %s\n", sv_from(proc->name), type_dbg_str(proc->type));

	for (pir_rblock_t i = 0; i < arrlenu(proc->blocks); i++) {
		pir_block_t *block = &proc->blocks[i];
		eprintf("%u:\n", i);
		for (pir_rinst_t j = block->first; j < block->first + block->len; j++) {
			pir_inst_t *inst = &proc->insts[j];

			eprintf("\t");
			_dump_inst(proc, inst);
		}
	}

	alloc_reset(sc);
}