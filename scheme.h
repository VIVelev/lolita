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
    char Cstring[8];
  } string;
  struct SCM_symbol {
    SCM pname;
  } symbol;
  struct SCM_box {
    SCM content;
  } box;
  struct SCM_closure {
    SCM (*behaviour)();
    long arity;
    SCM environment[1];
  } closure;
  struct SCM_escape {
    struct SCM_jmp_buf *stack_address;
  } escape;
};

enum SCM_tag {
  SCM_NULL_TAG = 0xaaa0,
  SCM_PAIR_TAG = 0xaaa1,
  SCM_BOOL_TAG = 0xaaa2,
  SCM_UNDEF_TAG = 0xaaa3,
  SCM_SYMBOL_TAG = 0xaaa4,
  SCM_STRING_TAG = 0xaaa5,
  SCM_SUBR_TAG = 0xaaa6,
  SCM_CLOSURE_TAG = 0xaaa7,
  SCM_ESCAPE_TAG = 0xaaa8,
};

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
    char Cstring[8];
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
  struct SCM_unwrapped_escape {
    union SCM_header header;
    struct SCM_jmp_buf *stack_address;
  } escape;
};

#define SCM_Wrap(x) ((SCM)(((union SCM_header *)x) + 1))
#define SCM_Unwrap(x) ((SCMRef)(((union SCM_header *)x) - 1))
#define SCM_2tag(x) ((SCM_Unwrap((SCM)x))->object.heaer.tag)
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

#define SCM_2bool(i) ((i) ? SCM_true : SCM_false)

#define SCM_Car(x) (SCM_Unwrap(x)->pair.car)
#define SCM_Cdr(x) (SCM_Unwrap(x)->pair.cdr)
#define SCM_NullP(x) (x == SCM_nil)
#define SCM_PairP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_PAIR_TAG))
#define SCM_SymbolP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_SYMBOL_TAG))
#define SCM_StringP(x) ((!SCM_FixnumP(x)) && (SCM_2tag(x) == SCM_STRING_TAG))
#define SCM_EqP(x, y) (x == y)
