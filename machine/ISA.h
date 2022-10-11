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

#define PLACE_BOOL 0x05
// Place boolean value 'b' on the stack.
// [s,_ => s,b,_]

#define PLACE_INT 0x06
// Place integer value 'i' on the stack.
// [s,_ => s,i,_]

#define CLONE_FULL 0x07
#define CLONE_HALF 0x08
#define CLONE_SHORT 0x09
#define CLONE_BYTE 0x0a
// Create and place a clone of some size (8,4,2,1), at the top of the stack, onto the stack
// [s,v,_ => s,v,v,_]

#define FETCH_BOOL 0x0b
// Load boolean value to the stack, from 'addr' on the heap.
// [s,addr,_ => s,b,_]

#define FETCH_INT 0x0c
// Load integer value to the stack, from 'addr' on the heap. (follow references until the heap is reached)
// [s,addr,_ => s,i,_]

#define DECLARE_BOOL 0x0d
// Allocate a boolean value on the heap, placing the resulting address on the stack. 
// The address is marked as having been created in this stackframe.
// [s,_ => s,addr,_]

#define DECLARE_INT 0x0e
// Allocate an integer value on the heap, placing the resulting address on the stack. 
// The address is marked as having been created in this stackframe.
// [s,_ => s,addr,_]

#define ASSIGN_BOOL 0x0f
// Assign the boolean value 'b' to the heap address 'addr'.
// [s,addr,b,_ => s,_]

#define ASSIGN_INT 0x10
// Assign the boolean value 'i' to the heap address 'addr'.
// [s,addr,i,_ => s,_]

#define INT_ADD 0x11
// Add the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x+y),_]

#define INT_MUL 0x12
// Multiply the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x*y),_]


#define INT_SUB 0x13
// Subtract the integer values 'x' and 'y', removing them from the stack, and placing the result on the stack.
// [s,x,y,_ => s,(x-y),_]

#define INT_EQ 0x14
// Checks if the integer values 'x' and 'y' are equal, removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x=y),_]

#define INT_LT 0x15
// Checks if the integer values 'x' is smaller than 'y', removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x'<'y),_]

#define BOOL_EQ 0x16
// Checks if the boolean values 'x' and 'y' are equal, removing them from the stack, and placing the resulting boolean on the stack.
// [s,x,y,_ => s,(x=y),_]

#define BOOL_NOT 0x17
// Negates the boolean value 'x', removing it from the stack, and placing the resulting boolean on the stack.
// [s,b,_ => s,(!b),_]

#define BOOL_AND 0x18
// Remove the boolean values 'x' and 'y' from the stack, placing 'true' on the stack, if 'x' and 'y' are both 'true', otherwise placing 'false' on the stack.
// [s,x,y,_ => s,(x&y),_]

#define BOOL_OR 0x19
// Remove the boolean values 'x' and 'y' from the stack, placing 'true' on the stack, if 'x' and/or 'y' are 'true', otherwise placing 'false' on the stack.
// [s,x,y,_ => s,(x|y),_]

#define GETSP 0x1a
// Place the stack pointer on the stack
// [s,_ => s,sp,_]

#define GETBP 0x1b
// Place the stack base pointer on the stack
// [s,_ => s,bp,_]

#define MODSP 0x1c
// Modify the stack pointer by 'x'
// [s,_ => s,_]

#define CLONE_FRAME 0x1d
// Creates a new stackframe containing the same variables as the current stackframe. 
// Except they are all marked as not being created in the new stackframe. 
// [s,r_0,b_0,v_0...v_x => s,r_0,b_0,v_0...v_x,r_1,b_2,v_0...v_x]

#define FETCH_ADDR 0x1e
// Fetch a variable heap address from 's_addr' on the stack
// [s,s_addr,_ => s,v_addr,_]

#define FREE_VAR 0x1f
// Free the allocation referenced at the stack address 's_addr'
// [s,s_addr,_ => s,_]

#define FREE_VARS 0x20

#define PRINT_VAR 0x21
// Print a variable
// [s,addr,_ => s,_]

#define PRINT_INT 0x22
// Print an int of the stack
// [s,i,_ => s,_]

#define PRINT_BOOL 0x23
// Print an int of the stack
// [s,b,_ => s,_]

#define STACK_FETCH 0x24

#define BP_FETCH 0x25