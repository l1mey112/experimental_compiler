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

void NORETURN err_with_pos(loc_t loc, const char *fmt, ...) {
	char buf[256];
	
	va_list args;
	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	fs_file_t *file = fs_filep(loc.file);

	snprintf(err_diag.err_string, sizeof(err_diag.err_string), "%s:%u:%u: %s", file->fp, loc.line_nr + 1, loc.col + 1, buf);
	longjmp(err_diag.unwind, 1);
}

void NORETURN err_without_pos(const char *fmt, ...) {
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

#define LINE_LEN_PADDING 30

static const char *_local_str(pir_proc_t *proc, pir_rlocal_t local) {
	char *s = (char *)alloc_scratch(0);

	// local = local(0)
	//         arg(0)

	u32 len;
	if (proc->locals[local].is_arg) {
		len = sprintf(s, "arg(%u)", local);
	} else {
		len = sprintf(s, "local(%u)", local);
	}

	return (const char *)alloc_scratch(len + 1);
}

static const char *_sym_str(sym_resolve_t sym) {
	// HAH, nice error idiot. two functions trying to use the same scratch buffer
	// fixed, used to be `alloc_scratch(0)`
	char *s = (char *)alloc_scratch(256);

	// sym = main.main
	// local = local(0)
	//         arg(0)

	if (sym.sym == (rsym_t)-1) {
		snprintf(s, 256, "%s", fs_module_symbol_sv(sym.d_unresolved.module, sym.d_unresolved.lit));
	} else {
		assert(0 && "TODO: implementing repr of resolved symbols");
		// eprintf("sym(%s)\n", fs_module_symbol_sv(inst->d_sym.d_unresolved.module, inst->d_sym.d_unresolved.lit));
	}

	return s;
}

static void _padding_to_size(u32 line_len) {
	if (line_len < LINE_LEN_PADDING) {
		eprintf("%*c", LINE_LEN_PADDING - line_len, ' ');
	}
}

static const char *_inst_str(pir_proc_t *proc, pir_rinst_t inst) {
	char *s = (char *)alloc_scratch(0);

	u32 len = sprintf(s, "%%%u", proc->insts[inst].id);

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
	// if (inst->kind != PIR_LSTORE && inst->kind != PIR_RETURN) {
	if (!(inst->type == TYPE_VOID || inst->type == TYPE_NORETURN)) {
		line_len += eprintf("%s = ", _inst_str(proc, inst->id));
	}
	switch (inst->kind) {
		case PIR_LLOAD: {
			line_len += eprintf("load ");
			if (inst->d_load.is_sym) {
				line_len += eprintf("%s", fs_module_symbol_sv(inst->d_load.sym.d_unresolved.module, inst->d_load.sym.d_unresolved.lit));
				_padding_to_size(line_len);
				if (inst->d_load.sym.sym == SYM_UNRESOLVED) {
					eprintf("; unresolved\n");
				} else {
					assert(0 && "TODO: implementing repr of resolved symbols");
				}
			} else {
				line_len += eprintf("%s", _local_str(proc, inst->d_load.local));
				_padding_to_size(line_len);
				eprintf("; def %s%s: %s\n", proc->locals[inst->d_load.local].is_mut ? "mut " : "let ", sv_from(proc->locals[inst->d_load.local].name), type_dbg_str(proc->locals[inst->d_load.local].type));
			}
			break;
		}
		case PIR_LSTORE: {
			const char *sstr = inst->d_store.is_sym ? _sym_str(inst->d_store.sym) : _local_str(proc, inst->d_store.local);
			line_len += eprintf("store %s, %s", sstr, _inst_str(proc, inst->d_store.src));
			if (!inst->d_store.is_sym) {
				_padding_to_size(line_len);
				eprintf("; def %s%s: %s\n", proc->locals[inst->d_store.local].is_mut ? "mut " : "let ", sv_from(proc->locals[inst->d_store.local].name), type_dbg_str(proc->locals[inst->d_store.local].type));
			} else {
				eprintf("\n");
			}
			break;
		}
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
		case PIR_JMP:
			eprintf("jmp :%u\n", inst->d_jmp);
			break;
		case PIR_IF:
			eprintf("if %s goto :%u else :%u\n", _inst_str(proc, inst->d_if.cond), inst->d_if.on_true, inst->d_if.on_false);
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

		// iterate over linked list
		u32 dbg_size = 0;
		for (pir_rinst_t j = block->first; j != (pir_rinst_t)-1; j = proc->insts[j].next) {
			pir_inst_t *inst = &proc->insts[j];

			eprintf("\t");
			_dump_inst(proc, inst);
			// reset
			alloc_reset(sc);
			dbg_size++;
		}
		assert(dbg_size == block->len && "block->len is incorrect");
	}
}

// allocates using alloc_* functions
const char *tok_dbg_str(token_t tok) {
	// handle identifiers

	u8 *p;

	bool requires_quotes = true;
	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .type = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	//                             : "unexpected `x`, expected `+=`"

	if (TOK_HAS_LIT(tok.type) && tok.lit == (istr_t)-1) {
		requires_quotes = false;
	}
	
	if (TOK_HAS_LIT(tok.type) && tok.lit != (istr_t)-1) {
		str = sv_from(tok.lit);
		len = strlen(str);
	}
    #define X(val, lit) \
		else if (val == tok.type) str = lit, len = strlen(lit);
    TOK_X_LIST
    #undef X

	if (requires_quotes) {
		p = alloc_scratch(len + 2 + 1);
		sprintf((char *)p, "`%s`", str);
	} else {
		p = alloc_scratch(len + 1);
		strcpy((char *)p, str);
	}

	return (const char *)p;
}

const char *tok_literal_representation(tok_t tok) {
	switch (tok) {
		#define X(val, lit) \
			case val: return lit;
		TOK_X_LIST
		#undef X
	}
	assert_not_reached();
}
