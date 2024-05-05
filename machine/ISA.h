char* instruction_to_string(int inst);
int string_to_instruction(char* str);

// '_', marks the stack pointer

#define HALT 0x00
// Halt the machine
// [s,_ => ]

#define STOP 0x01
// Stop current routine by, freeing all heap allocations marked as owned by the current routine, and returning to the previous stack frame.
// Unless there are no lower stack frames, in which case the machine halts.
// [s,r,b,v_0...v_x,_ => s,_]

#define CALL 0x02
// Place current stack base pointer and next instruction pointer, on the stack. 
// Create new stack frame, with 'count' amount of arguments from the stack, and changes the instruction pointer to 'addr'.
// The argument variables will in the new stackframe be marked as not having been created in the new stackframe.
// [s, v_0,...,v_c,v_count,_ => s,r,b,v_0...v_c,_]

#define GOTO 0x03
// Change the instruction pointer to the following 'word'
// [s,_ => s,_]

#define IF_TRUE 0x04
// Change the instruction pointer to the following 'word', if the boolean value 'true' is at the top of the stack, then remove the boolean
// [s,b,_ => s,_]

#define PLACE_BYTE 0x05
// Place single byte value 'b' on the stack.
// [s,_ => s,b,_]

#define PLACE_FULL 0x06
// Place 8B value 'i' on the stack.
// [s,_ => s,i,_]

#define CLONE_FULL 0x07
#define CLONE_HALF 0x08
#define CLONE_SHORT 0x09
#define CLONE_BYTE 0x0a
// Create and place a clone of some size (8,4,2,1), at the top of the stack, onto the stack
// [s,v,_ => s,v,v,_]

#define FETCH_FULL 0x0b
#define FETCH_HALF 0x0c
#define FETCH_SHORT 0x0d
#define FETCH_BYTE 0x0e
#define FIELD_FETCH 0x0f

#define DECLARE_FULL 0x10
#define DECLARE_HALF 0x11
#define DECLARE_SHORT 0x12
#define DECLARE_BYTE 0x13
#define DECLARE_STRUCT 0x14

#define WRITE_FULL 0x15
#define WRITE_HALF 0x16
#define WRITE_SHORT 0x17
#define WRITE_BYTE 0x18

#define ASSIGN_FULL 0x19
#define ASSIGN_HALF 0x1a
#define ASSIGN_SHORT 0x1b
#define ASSIGN_BYTE 0x1c
#define REF_ASSIGN 0x1d
#define FIELD_WRITE 0x1e
#define FIELD_ASSIGN 0x1f

#define INT_ADD 0x20
// Add the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x+y),_]

#define INT_MUL 0x21
// Multiply the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x*y),_]


#define INT_SUB 0x22
// Subtract the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x-y),_]

#define INT_DIV 0x23
#define INT_MOD 0x24

#define FULL_EQ 0x25
// Checks if the integer values 'x' and 'y' are equal, removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x=y),_]

#define INT_LT 0x26
// Checks if the integer values 'x' is smaller than 'y', removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x'<'y),_]

#define BOOL_EQ 0x27
// Checks if the boolean values 'x' and 'y' are equal, removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x=y),_]

#define BOOL_NOT 0x28
// Negates the boolean value 'x', removing it from the stack, and placing the resulting boolean on the stack.
// [s,b,_ => s,(!b),_]

#define BOOL_AND 0x29
// Remove the boolean values 'x' and 'y' from the stack, placing 'true' on the stack, if 'x' and 'y' are both 'true', otherwise placing 'false' on the stack.
// [s,x,y,_ => s,(x&y),_]

#define BOOL_OR 0x2a
// Remove the boolean values 'x' and 'y' from the stack, placing 'true' on the stack, if 'x' and/or 'y' are 'true', otherwise placing 'false' on the stack.
// [s,x,y,_ => s,(x|y),_]

#define GETSP 0x2b
// Place the stack pointer on the stack
// [s,_ => s,sp,_]

#define GETBP 0x2c
// Place the stack base pointer on the stack
// [s,_ => s,bp,_]

#define MODSP 0x2d
// Modify the stack pointer by 'x'
// [s,_ => s,_]

#define FREE_VAR 0x2e
// Free the allocation referenced at the stack address 's_addr'
// [s,s_addr,_ => s,_]

#define FREE_VARS 0x2f

#define PRINT_INT 0x30
// Print an int of the stack
// [s,i,_ => s,_]

#define PRINT_BOOL 0x31
// Print an int of the stack
// [s,b,_ => s,_]

#define STACK_FETCH 0x32

#define BP_FETCH 0x33

#define SIZE_OF 0x34

#define START 0x35

#define REF_FETCH 0x36

#define INCR_REF 0x37

#define PRINT_CHAR 0x38

#define GET_INPUT 0x39

#define HALF_EQ 0x3a
#define SHORT_EQ 0x3b
#define BYTE_EQ 0x3c