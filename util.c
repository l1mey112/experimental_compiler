#include "all.h"

#include <setjmp.h>
#include "stb_ds.h"

void err_with_pos(loc_t loc, const char *fmt, ...) {
	char buf[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	file_entry_t *file = &file_entries[loc.file];

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
static u8 scratch_buf[8192];
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

static void _dump_inst(hir_proc_t *proc, hir_inst_t *inst) {
	switch (inst->kind) {
		case HIR_ARG:
			eprintf("%%%u = arg(l%u:%s)\n", inst->id, inst->d_arg.local, sv_from(proc->locals[inst->d_arg.local].name));
			break;
		case HIR_LOCAL_GET:
		case HIR_LOCAL_SET:
		case HIR_LOCAL_REF:
		case HIR_COPY:
		case HIR_PHI:
		case HIR_CALL:
		case HIR_JMP:
		case HIR_RET:
		case HIR_OP1:
		case HIR_OP2:
			assert_not_reached();
	}
}

void dump_proc(hir_proc_t *proc) {
	u8 *sc = alloc_scratch(0);

	eprintf("%s: %s\n", sv_from(proc->name), table_type_dbg_str(proc->type));

	for (hir_rblock i = 0; i < arrlenu(proc->blocks); i++) {
		hir_block_t *block = &proc->blocks[i];
		eprintf("%u:\n", i);
		for (hir_rinst j = block->first; j < block->first + block->len; j++) {
			hir_inst_t *inst = &proc->insts[j];

			eprintf("\t");
			_dump_inst(proc, inst);
		}
	}

	alloc_reset(sc);
}