open ProgramRep

let definitions = "
typedef signed long long int full_t;
typedef unsigned long long int ufull_t;
typedef signed int half_t;
typedef unsigned int uhalf_t;
typedef signed short int short_t;
typedef unsigned short int ushort_t;
typedef signed char byte_t;
typedef unsigned char ubyte_t;
#define STACK_SIZE 10000
"

let includes = "
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
"

let variables = "
ufull_t sp = 0;
ufull_t bp = 0;
ufull_t depth = 0;
byte_t s[STACK_SIZE];
byte_t* heap_min = (byte_t*)ULONG_MAX;
byte_t* heap_max = (byte_t*)0;
void* next_label;
"

let functions = "
static inline byte_t*  allocate(unsigned int size) {
    byte_t* alloc = (byte_t*)malloc(8+size);
    if (alloc+size+8 > heap_max) heap_max = alloc+8;
    if (alloc < heap_min) heap_min = alloc;

    memset(alloc+8, 0, size);

    *((uhalf_t*)alloc) = 0;
    *(((uhalf_t*)alloc)+1) = ((uhalf_t)size << 1);

    return alloc+8;
}

void read_input(unsigned int max_size, char** ret) {
    char buffer[max_size + 1];
    char ch = 0;
    unsigned int count = 0;
    while(ch != '\\n' && count < max_size) {
        ch = getchar();
        buffer[count++] = ch;
    } 
    buffer[count-1] = '\\0';
    *ret = buffer;
}

static inline int on_heap(full_t* target) {
    return ((byte_t*)target >= heap_min && (byte_t*)target < heap_max);
}

static inline int on_stack(full_t* target) {
    return ((byte_t*)target >= s && (byte_t*)target < (s+STACK_SIZE));
}

static inline byte_t to_origin(full_t** target, ufull_t sp) {
    if (**target == 0) return 1;
    if (!on_heap((full_t*)**target) && !on_stack((full_t*)**target)) return 0;
    if (on_heap((full_t*)**target)) return 1;
    if (on_stack((full_t*)**target)) {
        full_t temp = **target;
        while(1) {
            if (*(byte_t**)temp == 0) break;
            if (on_heap(*(full_t**)temp)) break;
            temp = *(full_t*)temp;
        }
        *target = (full_t*)temp;
        return 1;
    }
}

static inline void try_free(full_t* addr, unsigned int depth) {
    if(!on_heap((full_t*)addr) || !on_stack((full_t*)addr)) return;
    if (*addr == 0) return;
    to_origin(&addr, sp);
    if (!on_heap((full_t*)addr)) addr = (full_t*)*addr;

    (((uhalf_t *)addr)[-2] = ((uhalf_t *)addr)[-2] - 1);
    if ((((ufull_t *)addr)[-2])) return;
    if ((((uhalf_t *)addr)[-1] & 1)) {
        unsigned int fields = ((((uhalf_t *)addr)[-1]) >> 1);
        full_t** point = (full_t**)addr;
        for(int i = 0; i < fields; i++) try_free(*(point + i), depth+1);
    }

    free(addr-1);
}

byte_t* allocate_struct(unsigned int fields) {
    unsigned int total_size = 8+(fields*8);
    byte_t* alloc = (byte_t*)malloc(total_size);
    if (alloc+total_size > heap_max) heap_max = alloc+total_size;
    if (alloc < heap_min) heap_min = alloc;

    memset(((ufull_t*)alloc)+1, 0, (fields*8));

    
    *((uhalf_t*)alloc) = 0; 
    *(((uhalf_t*)alloc)+1) = ((((uhalf_t)fields) << 1) | 1); 

    return alloc+8;
}

static inline void declare_f() {
    *(full_t*)(s + sp) = (full_t)((full_t*)allocate(8));
    sp += 8;
}

static inline void declare_h() {
    *(full_t*)(s + sp) = (full_t)((full_t*)allocate(4));
    sp += 8;
}

static inline void declare_s() {
    *(full_t*)(s + sp) = (full_t)((full_t*)allocate(2));
    sp += 8;
}

static inline void declare_b() {
    *(full_t*)(s + sp) = (full_t)((full_t*)allocate(1));
    sp += 8;
}

static inline void declare_struct() {
    ufull_t fields = *(ufull_t*)(s + sp + -8);
    byte_t* alloc = allocate_struct(fields);
    *(full_t*)(s + sp + -8) = (full_t)((full_t*)alloc);
}

static inline void clone_f() {
    full_t value = *(full_t*)(s + sp + -8);
    *(full_t*)(s + sp) = value;
    sp += 8;
}

static inline void clone_h() {
    half_t value = *(half_t*)(s + sp + -4);
    *(half_t*)(s + sp) = value;
    sp += 4;
}

static inline void clone_s() {
    short_t value = *(short_t*)(s + sp + -2);
    *(short_t*)(s + sp) = value;
    sp += 2;
}

static inline void clone_b() {
    byte_t value = *(byte_t*)(s + sp + -1);
    *(byte_t*)(s + sp) = value;
    sp += 1;
}

static inline void place_f(full_t value) {
    *(full_t*)(s + sp) = value;
    sp += 8;
}

static inline void place_b(byte_t value) {
    *(byte_t*)(s + sp) = value;
    sp += 1;
}

static inline void print_int() {
    full_t value = *(full_t*)(s + sp + -8);
    printf(\"%llu\", value);
    sp -= 8;
}

static inline void print_char() {
    byte_t value = *(byte_t*)(s + sp + -1);
    printf(\"%c\", value);
    sp -= 1;
}

static inline void print_bool() {
  byte_t value = *(byte_t*)(s + sp + -1);
  if (value) printf(\"true\");
  else printf(\"false\");
  sp -= 1;
}

static inline void assign_f() {
    full_t* target = *(full_t**)(s + sp + -16);
    full_t value = *(full_t*)(s + sp + -8);
    *target = value;
    sp -= 16;
}

static inline void assign_h() {
    full_t* target = *(full_t**)(s + sp + -12);
    full_t value = *(full_t*)(s + sp + -4);
    *target = value;
    sp -= 12;
}

static inline void assign_s() {
    full_t* target = *(full_t**)(s + sp + -10);
    full_t value = *(full_t*)(s + sp + -2);
    *target = value;
    sp -= 10;
}

static inline void assign_b() {
    full_t* target = *(full_t**)(s + sp + -9);
    full_t value = *(full_t*)(s + sp + -1);
    *target = value;
    sp -= 9;
}

static inline void ref_assign() {
    full_t** target = *(full_t***)(s + sp + -16);
    full_t* value = *(full_t**)(s + sp + -8);

    try_free(*target, 0);

    *target = value;
    sp -= -16;
}

static inline void field_assign() {
    ufull_t** target = *(ufull_t***)(s + sp + -24);
    ufull_t offset = *(ufull_t*)(s + sp + -16);
    full_t* value = *(ufull_t**)(s + sp + -8);

    try_free(*(target + offset), 0);

    *(target + offset) = value;
    sp -= 24;
}

static inline void ref_fetch() {
    full_t* target = *(full_t**)(s + sp + -8);
    if (on_stack(target)) to_origin(&target, sp);
    *(full_t**)(s + sp + -8) = target;
}

static inline void field_fetch() {
    ufull_t offset = *(ufull_t*)(s + sp + -8);
    full_t** target = *((full_t***)(s + sp + -16));

    if (!(((uhalf_t *)target)[-1] & 1)) { printf(\"Failure: Field fetch from non-struct data\\n\"); exit(1); } 
    if (((((uhalf_t *)target)[-1]) >> 1) <= offset) { printf(\"Failure: Field fetch out of struct bounds\\n\"); exit(1); } 

    *(full_t***)(s + sp + -16) = target + offset;
    sp -= 8;
}

static inline void fetch_f() {
    full_t* target = *(full_t**)(s + sp + -8);
    *(full_t*)(s + sp + -8) = *((full_t*)target);
}

static inline void fetch_h() {
    full_t* target = *(full_t**)(s + sp + -8);
    *(half_t*)(s + sp + -8) = *((half_t*)target);
    sp -= 4;
}

static inline void fetch_s() {
    full_t* target = *(full_t**)(s + sp + -8);
    *(short_t*)(s + sp + -8) = *((short_t*)target);
    sp -= 6;
}

static inline void fetch_b() {
    full_t* target = *(full_t**)(s + sp + -8);
    *(byte_t*)(s + sp + -8) = *((byte_t*)target);
    sp -= 7;
}

static inline void stack_fetch(full_t offset) {
    full_t* value = (full_t*)(s + (8*offset));
    *(full_t*)(s + sp) = (full_t)value;
    sp += 8;
}

static inline void bp_fetch(full_t offset) {
    full_t* value = (full_t*)(s + bp + (8*offset));
    *(full_t*)(s + sp) = (full_t)value;
    sp += 8;
}

static inline void eq_f() {
    byte_t eq = (*(full_t*)(s + sp + -8)) == (*(full_t*)(s + sp + -16));
    sp -= 16;
    *(byte_t*)(s + sp) = eq; 
    sp += 1;
}

static inline void eq_h() {
    byte_t eq = (*(half_t*)(s + sp + -4)) == (*(half_t*)(s + sp + -8));
    sp -= 8;
    *(byte_t*)(s + sp) = eq; 
    sp += 1;
}

static inline void eq_s() {
    byte_t eq = (*(short_t*)(s + sp + -2)) == (*(short_t*)(s + sp + -4));
    sp -= 4;
    *(byte_t*)(s + sp) = eq; 
    sp += 1;
}

static inline void eq_b() {
    byte_t eq = (*(byte_t*)(s + sp + -1)) == (*(byte_t*)(s + sp + -2));
    sp -= 2;
    *(byte_t*)(s + sp) = eq; 
    sp += 1;
}

static inline void bool_eq() {
    byte_t eq = !!(*(s + sp + -1)) == !!(*(s + sp + -2));
    *(byte_t*)(s + sp + -2) = eq;
    sp -= 1;
}

static inline void bool_not() {
    *(byte_t*)(s + sp + -1) = !(*(s + sp + -1));
}

static inline void bool_and() {
    byte_t res = (*(s + sp + -1)) && (*(s + sp + -1));
    *(byte_t*)(s + sp + -2) = res;
    sp -= 1;
}

static inline void bool_or() {
    byte_t res = (*(s + sp + -1)) || (*(s + sp + -1));
    *(byte_t*)(s + sp + -2) = res;
    sp -= 1;
}

static inline void int_mul() {
    full_t value = (*(full_t*)(s + sp + -8)) * (*(full_t*)(s + sp + -16));
    *(full_t*)(s + sp + -16) = value;
    sp -= 8;
}

static inline void int_add() {
    full_t value = (*(full_t*)(s + sp + -8)) + (*(full_t*)(s + sp + -16));
    *(full_t*)(s + sp + -16) = value;
    sp -= 8;
}

static inline void int_sub() {
    full_t value = (*(full_t*)(s + sp + -8)) - (*(full_t*)(s + sp + -16));
    *(full_t*)(s + sp + -16) = value;
    sp -= 8;
}

static inline void incr_ref() {
    full_t* target = *(full_t**)(s + sp + -8);
    if (target) {
        if (on_heap(target)) {
            ((uhalf_t*)target)[-2] = (((uhalf_t*)target)[-2] + 1);
        }
        else if (on_stack(target)) {
            to_origin(&target, sp);
            if (*target) {
                ((uhalf_t*)target)[-2] = (((uhalf_t*)target)[-2] + 1);
            }
        }
    }
}

static inline void free_var() {
    full_t* target = *(full_t**)(s + sp + -8);
    try_free(target, 0);
    sp -= 8;
}

static inline void* call(void* ret) {
    full_t arg_count = *(full_t*)(s + sp + -16);
    full_t target = *(full_t*)(s + sp + -8);

    for(int i = 0; i < arg_count; i++) {
        full_t* arg = *(full_t**)(s + sp - (8*(i+3)));
        *(full_t*)(s + sp - (8*(i+1))) = (full_t)(arg);
    }

    *(full_t*)((s + sp) - (8*(arg_count+2))) = (full_t)ret;
    *(full_t*)((s + sp) - (8*(arg_count+1))) = (full_t)bp;

    depth++;
    bp = sp - (8*arg_count);
    return (void*)target;
}

static inline void size_of() {
    full_t* target = *(full_t**)(s + sp + -8);
    *(full_t*)(s + sp + -8) = ((((uhalf_t *)target)[-1]) >> 1);
}

static inline void read_int() {
    char* int_buffer;
    read_input(20, &int_buffer);
    ufull_t int_value = atoi(int_buffer);
    *(ufull_t*)(s + sp) = int_value;
    sp += 8;
}

static inline void read_bool() {
    char* bool_buffer;
    read_input(10, &bool_buffer);
    byte_t bool_value; 
    if (strcmp(bool_buffer, \"true\") == 0) bool_value = 1;
    else if (strcmp(bool_buffer, \"false\") == 0) bool_value = 0;
    else { printf(\"Failure: Expected a 'bool' but got: %s\\n\", bool_buffer); exit(1); }
    *(byte_t*)(s + sp) = bool_value;
    sp += 1;
}

static inline void read_char() {
    char* char_buffer;
    read_input(3, &char_buffer);
    byte_t char_value;
    if (char_buffer[0] == '\\\\') {
        if (char_buffer[2] != 0) { printf(\"Failure: Expected a 'char' but got: %s\\n\", char_buffer); exit(1); }
        switch (char_buffer[1]) {
            case 'n': char_value = '\\n';
            default: { printf(\"Failure: Expected a 'char' but got: %s\\n\", char_buffer); exit(1); }
        }
    }
    else {
        if (char_buffer[1] != 0) { printf(\"Failure: Expected a 'char' but got: %s\\n\", char_buffer); exit(1); }
        char_value = char_buffer[0];
    }
    *(byte_t*)(s + sp) = char_value;
    sp += 1;
}

static inline void* stop() {
    if (depth == 0) {
        exit(0);
    }

    ufull_t i = sp - 8;
    while (i >= bp) {
        try_free(*((full_t**)(s+i)), 0);
        i -= 8;
    }

    depth--;
    ufull_t old_bp = *(full_t*)(s + bp + -8);
    ufull_t next_ip = *(full_t*)(s + bp + -16);
    sp = bp - 16;
    bp = old_bp;

    return (void*)next_ip;
}

"


let main = "

int main(int argc, char *argv[]) {
  if (argc < 2) { printf(\"No entrypoint specified\\n\"); exit(1); }
  program(argv[1]);
  return 1;
}
"

let wrap_program str = "void program(char* entry){\n"^str^"}"

let translate_program_part_to_c pp cnt = match pp with
  | CLabel s -> "label_"^s^":\n"
  | CHalt -> "exit(0);\n"
  | CStop -> "next_label = stop(); goto *next_label;\n"
  | CPlaceLabel s -> "place_f((full_t)&&label_"^s^");\n"
  | Call -> ( 
    let call_label = "call_label_"^string_of_int cnt in
    "next_label = call(&&"^call_label^"); goto *next_label;\n"^call_label^":\n"
  )
  | GoTo s -> "goto label_"^s^";\n"
  | IfTrue s -> "sp -= 1; if ((*(byte_t*)(s + sp))) goto label_"^s^";\n"
  | PlaceByte bc -> ( match bc with
    | C_Bool true -> "place_b(1);\n"
    | C_Bool false -> "place_b(0);\n"
    | C_Char c -> "place_b("^(Char.code c |> string_of_int)^");\n"
  )
  | PlaceFull fc -> ( match fc with
    | C_Int i -> "place_f("^string_of_int i^");\n"
  )
  | CloneFull -> "clone_f();\n"
  | CloneHalf -> "clone_h();\n"
  | CloneShort -> "clone_s();\n"
  | CloneByte -> "clone_b();\n"
  | FetchFull -> "fetch_f();\n"
  | FetchHalf -> "fetch_h();\n"
  | FetchShort -> "fetch_s();\n"
  | FetchByte -> "fetch_b();\n"
  | FieldFetch -> "field_fetch();\n"
  | DeclareFull -> "declare_f();\n"
  | DeclareHalf -> "declare_h();\n"
  | DeclareShort -> "declare_s();\n"
  | DeclareByte -> "declare_b();\n"
  | DeclareStruct -> "declare_struct();\n"
  | AssignFull -> "assign_f();\n"
  | AssignHalf -> "assign_h();\n"
  | AssignShort -> "assign_s();\n"
  | AssignByte -> "assign_b();\n"
  | RefAssign -> "ref_assign();\n"
  | FieldAssign -> "field_assign();\n"
  | IntAdd -> "int_add();\n"
  | IntMul -> "int_mul();\n"
  | IntSub -> "int_sub();\n"
  | FullEq -> "eq_f();\n"
  | IntLt -> "int_lt();\n"
  | BoolEq -> "bool_eq();\n"
  | BoolNot -> "bool_not();\n"
  | BoolAnd -> "bool_and();\n"
  | BoolOr -> "bool_or();\n"
  | GetSP -> "get_sp();\n"
  | GetBP -> "get_bp();\n"
  | ModSP i -> "sp += "^string_of_int i^";\n"
  | FreeVar -> "free_var();\n"
  | FreeVars i -> List.init i (fun _ -> "free_var();\n") |> String.concat ""
  | PrintInt -> "print_int();\n"
  | PrintBool -> "print_bool();\n"
  | StackFetch i -> "stack_fetch("^string_of_int i^");\n"
  | BPFetch i -> "bp_fetch("^string_of_int i^");\n"
  | SizeOf -> "size_of();\n"
  | Start -> "void* entry_label = start(entry); goto *entry_label;\n"
  | RefFetch -> "ref_fetch();\n"
  | IncrRef -> "incr_ref();\n"
  | PrintChar -> "print_char();\n"
  | GetInput i -> ( match i with
    | 0 -> "read_int();\n" (* T_Int *)
    | 1 -> "read_bool();\n" (* T_Bool *)
    | 2 -> "read_char();\n" (* T_Char *)
    | _ -> "Not inputable type"
  )
  | HalfEq -> "eq_h();\n"
  | ShortEq -> "eq_s();\n"
  | ByteEq -> "eq_b();\n"

let create_starter gs = 
  let create_if_case name cnt = "if (strcmp(entry, \""^name^"\") == 0) return **(((void***)s)+"^string_of_int cnt^");\n" in
  let rec aux gs acc cnt = match gs with
  | [] -> acc
  | h::t -> ( match h with
    | (Entry,_,T_Routine(_,_),_,name) -> aux t (create_if_case name cnt::acc) (cnt+1)
    | _ -> aux t acc (cnt+1)
  )
  in
  let inner = aux gs ["{ printf(\"No such entry point: %s\\n\", entry); exit(1); }"] 0 |> String.concat "" in
  "static inline void* start(char* entry) {\nbp = sp;\n" ^inner ^ "\n}\n\n"

let transpile_to_c (Program(_,gs,p)) =
  let program = List.mapi (fun i p -> translate_program_part_to_c p i ) p |> String.concat "" in
  definitions ^ includes ^ variables ^ functions ^ create_starter gs ^ wrap_program program ^ main