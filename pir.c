#include "all.h"

#include "stb_ds.h"

pir_inst_t *pir_at(pir_proc_t *proc, pir_rinst_t inst) {
	return &proc->insts[inst];
}

pir_rinst_t pir_insert_after(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at, pir_inst_t inst) {
	pir_rinst_t id = arrlenu(proc->insts);
	inst.id = id;
	inst.next = (pir_rinst_t)-1;
	inst.prev = (pir_rinst_t)-1;
	arrpush(proc->insts, inst);

	pir_block_t *bb = &proc->blocks[bb_ref];
	bb->len++;

	if (bb->first == (pir_rinst_t)-1) {
		bb->first = id;
		bb->last = id;
	} else {
		pir_inst_t *at_inst = pir_at(proc, at);

		if (at == bb->last) {
			bb->last = id;
		}

		if (at_inst->next != (pir_rinst_t)-1) {
			pir_inst_t *next_inst = pir_at(proc, at_inst->next);
			next_inst->prev = id;
		}

		at_inst->next = id;
		
		inst.prev = at;
		inst.next = at_inst->next;
	}

	return id;
}

pir_rinst_t pir_insert_before(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at, pir_inst_t inst) {
	pir_rinst_t id = arrlenu(proc->insts);
	inst.id = id;
	inst.next = (pir_rinst_t)-1;
	inst.prev = (pir_rinst_t)-1;
	arrpush(proc->insts, inst);

	pir_block_t *bb = &proc->blocks[bb_ref];
	bb->len++;

	if (bb->first == (pir_rinst_t)-1) {
		bb->first = id;
		bb->last = id;
	} else {
		pir_inst_t *at_inst = pir_at(proc, at);

		if (at == bb->first) {
			bb->first = id;
		}

		if (at_inst->next != (pir_rinst_t)-1) {
			pir_inst_t *prev_inst = pir_at(proc, at_inst->prev);
			prev_inst->next = id;
		}

		at_inst->prev = id;

		inst.prev = at_inst->prev;
		inst.next = at;
	}

	return id;
}

pir_rinst_t pir_insert(pir_proc_t *proc, pir_rblock_t bb_ref, pir_inst_t inst) {
	return pir_insert_after(proc, bb_ref, proc->blocks[bb_ref].last, inst);
}

// TODO: some novel free list impl? i have notes on this.
void pir_delete(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at) {
	pir_inst_t *inst = pir_at(proc, at);
	pir_block_t *bb = &proc->blocks[bb_ref];

	bb->len--;
	if (bb->first == at) {
		bb->first = inst->next;
	}
	if (bb->last == at) {
		bb->last = inst->prev;
	}
	if (inst->prev != (pir_rinst_t)-1) {
		pir_inst_t *prev_inst = pir_at(proc, inst->prev);
		prev_inst->next = inst->next;
	}
	if (inst->next != (pir_rinst_t)-1) {
		pir_inst_t *next_inst = pir_at(proc, inst->next);
		next_inst->prev = inst->prev;
	}

	inst->kind = PIR_NOP;
}

pir_inst_t pir_pop(pir_proc_t *proc, pir_rblock_t bb_ref, pir_rinst_t at) {
	pir_inst_t inst = *pir_at(proc, at);
	pir_delete(proc, bb_ref, at);
	return inst;
}

pir_rblock_t pir_new_block(pir_proc_t *proc) {
	pir_rblock_t id = arrlenu(proc->blocks);
	pir_block_t block = {
		.id = id,
		.first = (pir_rinst_t)-1,
		.last = (pir_rinst_t)-1,
		.len = 0,
	};
	arrpush(proc->blocks, block);
	return id;
}