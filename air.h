#pragma once

#include "shared.h"

typedef struct hair_inst_t hair_inst_t;
typedef struct hair_arg_t hair_arg_t;
typedef enum hair_op_t hair_op_t;

enum hair_type_t {
	HAIR_TYPE_i1,
	HAIR_TYPE_i8,
	HAIR_TYPE_i16,
	HAIR_TYPE_i32,
	HAIR_TYPE_i64,
	// TODO: floats?
};

// https://github.com/WebKit/WebKit/blob/main/Source/JavaScriptCore/b3/air/AirOpcode.opcodes
// impl preindex and postindex

// tmp is local, address is a memory location (ptr)

enum hair_op_t {
	HAIR_OP_ADD,
	HAIR_OP_SUB,
	HAIR_OP_MUL,
	HAIR_OP_DIV,
	HAIR_OP_NEG,
	// HAIR_OP_LEA, // lower into an LEA or generate an LEA
	HAIR_OP_LSHIFT,
	HAIR_OP_RSHIFT,
	HAIR_OP_URSHIFT,
	// TODO: rotate intrins
	HAIR_OP_OR,
	HAIR_OP_XOR,
	HAIR_OP_AND,
	HAIR_OP_NOT,
	// HAIR_OP_ABS, // could be an intrinsic?
	// TODO: casting operations, int to float, float to int
	HAIR_OP_MOV, // mov to address or tmp
	// HAIR_OP_CMOV // yeah?
	HAIR_OP_RETURN, // return multireturn
	HAIR_OP_CALL, // has FLAGS (can call from a table - may be noreturn)
};

struct hair_arg_t {
	// temporary -- register or stack slot
	// immediate -- 10239
	// addr      -- [tmp + 8]
	bool is_signed;
};

struct hair_inst_t {
	hair_arg_t arg[3];
	hair_op_t op;
	// *debuginfo
	// bool effects; // either will trap or perform control effects
	// bool spill; // was generated for stack spilling
};


