#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define SCM_FixnumP(x) ((unsigned long)(x) & (unsigned long)1)
#define SCM_Fixnum2int(x) ((long)(x) >> 1)
#define SCM_Int2fixnum(i) ((SCM)((i << 1) | 1))

typedef union SCM_object *SCM;
typedef union SCM_unwrapped_object *SCMRef;

union SCM_object {
  struct SCM_pair {
    SCM cdr;
    SCM car;
  } pair;
  struct SCM_string {
    char Cstring[1];
  } string;
  struct SCM_symbol {
    SCM pname;
  } symbol;
  struct SCM_closure {
    SCM (*behaviour)();
    long arity;
    SCM environment[1];
  } closure;
};

enum SCM_tag {
  SCM_NULL_TAG = 0xaaa0,
  SCM_PAIR_TAG = 0xaaa1,
  SCM_BOOL_TAG = 0xaaa2,
  SCM_UNDEF_TAG = 0xaaa3,
  SCM_SYMBOL_TAG = 0xaaa4,
  SCM_STRING_TAG = 0xaaa5,
  SCM_CLOSURE_TAG = 0xaaa6,
};

enum SCM_err {
  SCM_ERR_CANT_ALLOC = 1,
  SCM_ERR_NOT_CALLABLE = 2,
  SCM_ERR_INCORRECT_ARITY = 3,
};

#define SCM_error(code) SCM_signal_error(code, __LINE__, __FILE__)
SCM SCM_signal_error(unsigned long code, unsigned long line, char *file) {
  fflush(stdout);
  fprintf(stderr, "Error %lu, Line %lu, File %s. \n", code, line, file);
  exit(code);
}

union SCM_header {
  enum SCM_tag tag;
  SCM ignore;
};

union SCM_unwrapped_object {
  struct SCM_unwrapped_direct_object {
    union SCM_header header;
  } object;
  struct SCM_unwrapped_pair {
    union SCM_header header;
    SCM car;
    SCM cdr;
  } pair;
  struct SCM_unwrapped_string {
    union SCM_header header;
    char Cstring[1];
  } string;
  struct SCM_unwrapped_symbol {
    union SCM_header header;
    SCM pname;
  } symbol;
  struct SCM_unwrapped_closure {
    union SCM_header header;
    SCM (*behaviour)();
    long arity;
    SCM environment[1];
  } closure;
};

#define SCM_Wrap(x) ((SCM)(((union SCM_header *)x) + 1))
#define SCM_Unwrap(x) ((SCMRef)(((union SCM_header *)x) - 1))
#define SCM_2tag(x) ((SCM_Unwrap((SCM)x))->object.header.tag)
#define SCM_CfunctionAddress(Cfunction) ((SCM(*)(void))Cfunction)

#define SCM_DefinePair(pair, car, cdr)                                         \
  static struct SCM_unwrapped_pair pair = {{SCM_PAIR_TAG}, car, cdr}
#define SCM_DefineSymbol(symbol, pname)                                        \
  static struct SCM_unwrapped_symbol symbol = {{SCM_SYMBOL_TAG}, pname}
#define SCM_DefineString(Cname, string)                                        \
  struct Cname##_struct {                                                      \
    union SCM_header header;                                                   \
    char Cstring[sizeof(string) + 1];                                          \
  };                                                                           \
  static struct Cname##_struct Cname = {{SCM_STRING_TAG}, string}
#define SCM_DefineDirectObject(name, tag)                                      \
  struct SCM_unwrapped_direct_object name = {{tag}}
SCM_DefineDirectObject(SCM_true_object, SCM_BOOL_TAG);
SCM_DefineDirectObject(SCM_false_object, SCM_BOOL_TAG);
SCM_DefineDirectObject(SCM_nil_object, SCM_NULL_TAG);
#define SCM_true SCM_Wrap(&SCM_true_object)
#define SCM_false SCM_Wrap(&SCM_false_object)
#define SCM_nil SCM_Wrap(&SCM_nil_object)

#define SCM_DefineClosure(Cname, fields)                                       \
  struct Cname {                                                               \
    SCM (*behaviour)(void);                                                    \
    long arity;                                                                \
    fields                                                                     \
  }

#define SCM_DeclareFunction(Cname)                                             \
  SCM Cname(struct Cname *self_, unsigned long size_, va_list arguments_)
#define SCM_DeclareLocalVariable(Cname) SCM Cname = va_arg(arguments_, SCM)
#define SCM_Free(Cname) (self_->Cname)

#define SCM_2bool(i) ((i) ? SCM_true : SCM_false)

#define SCM_Car(x) (SCM_Unwrap(x)->pair.car)
#define SCM_Cdr(x) (SCM_Unwrap(x)->pair.cdr)
#define SCM_NullP(x) (x == SCM_nil)
#define SCM_PairP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_PAIR_TAG))
#define SCM_SymbolP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_SYMBOL_TAG))
#define SCM_StringP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_STRING_TAG))
#define SCM_EqP(x, y) (x == y)

SCM SCM_cons(SCM x, SCM y) {
  SCMRef cell = (SCMRef)malloc(sizeof(struct SCM_unwrapped_pair));
  if (cell == (SCMRef)NULL)
    SCM_error(SCM_ERR_CANT_ALLOC);
  cell->pair.header.tag = SCM_PAIR_TAG;
  cell->pair.car = x;
  cell->pair.cdr = x;
  return SCM_Wrap(cell);
}

SCM SCM_close(SCM (*Cfunction)(void), long arity, unsigned long size, ...) {
  SCMRef result = (SCMRef)malloc(sizeof(struct SCM_unwrapped_closure) +
                                 (size - 1) * sizeof(SCM));
  unsigned long i;
  va_list args;
  if (result == (SCMRef)NULL)
    SCM_error(SCM_ERR_CANT_ALLOC);
  result->closure.header.tag = SCM_CLOSURE_TAG;
  result->closure.arity = arity;
  result->closure.behaviour = Cfunction;
  va_start(args, size);
  for (i = 0; i < size; i++)
    result->closure.environment[i] = va_arg(args, SCM);
  va_end(args);
  return SCM_Wrap(result);
}

SCM SCM_invoke(SCM f, unsigned long arity, ...) {
  SCMRef func = SCM_Unwrap(f);
  if (func->object.header.tag != SCM_CLOSURE_TAG)
    SCM_error(SCM_ERR_NOT_CALLABLE);

  if (func->closure.arity != arity)
    SCM_error(SCM_ERR_INCORRECT_ARITY);

  va_list args;
  va_start(args, arity);
  SCM (*behaviour)(struct SCM_closure *, unsigned long, va_list);
  behaviour = (SCM(*)(struct SCM_closure *, unsigned long,
                      va_list))func->closure.behaviour;
  SCM result = behaviour((struct SCM_closure *)f, arity, args);
  va_end(args);
  return result;
}

SCM SCM_print(SCM obj) {
  if (SCM_FixnumP(obj)) {
    printf("%ld", SCM_Fixnum2int(obj));
  } else if (SCM_NullP(obj)) {
    printf("()");
  } else if (SCM_PairP(obj)) {
    printf("(");
    SCM_print(SCM_Car(obj));
    SCM current = SCM_Cdr(obj);
    while (SCM_PairP(current)) {
      printf(" ");
      SCM_print(SCM_Car(current));
      current = SCM_Cdr(current);
    }
    if (!SCM_NullP(current)) {
      printf(" . ");
      SCM_print(current);
    }
    printf(")");
  } else if (SCM_SymbolP(obj)) {
    SCM_print(SCM_Unwrap(obj)->symbol.pname);
  } else if (SCM_StringP(obj)) {
    printf("\"%s\"", SCM_Unwrap(obj)->string.Cstring);
  } else if (SCM_EqP(obj, SCM_true)) {
    printf("#t");
  } else if (SCM_EqP(obj, SCM_false)) {
    printf("#f");
  } else if (SCM_2tag(obj) == SCM_CLOSURE_TAG) {
    printf("#<procedure>");
  } else {
    printf("#<unknown>");
  }
  return SCM_nil; // Convention: return SCM_nil after printing
}
