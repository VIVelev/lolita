/* Compiler to C, Version 0.1 */
/* Source expression:
  (begin (defmacro (aif test-form then-form else-form) (quasiquote ((lambda (it)
  (if it (unquote then-form) (unquote else-form))) (unquote test-form))))
  ((lambda (test) (aif (test) it (quote false-branch))) (lambda () (quote
  some-complex-computation) (quote a-truthy)))) */

#include "scheme.h"

/* Quotations */
SCM_DefineString(thing4_object, "a-truthy");
#define thing4 SCM_Wrap(&thing4_object)
SCM_DefineSymbol(thing3_object, thing4); /* a-truthy */
#define thing3 SCM_Wrap(&thing3_object)
SCM_DefineString(thing5_object, "some-complex-computation");
#define thing5 SCM_Wrap(&thing5_object)
SCM_DefineSymbol(thing2_object, thing5); /* some-complex-computation */
#define thing2 SCM_Wrap(&thing2_object)
SCM_DefineString(thing6_object, "false-branch");
#define thing6 SCM_Wrap(&thing6_object)
SCM_DefineSymbol(thing1_object, thing6); /* false-branch */
#define thing1 SCM_Wrap(&thing1_object)
#define thing0 SCM_true

/* Functions */
SCM_DefineClosure(function0, );

SCM_DeclareFunction(function0) {
  SCM_DeclareLocalVariable(it);
  return it != SCM_false ? it : thing1;
}

SCM_DefineClosure(function1, );

SCM_DeclareFunction(function1) {
  SCM_DeclareLocalVariable(test);
  return SCM_invoke(SCM_close(SCM_CfunctionAddress(function0), 1, 0), 1,
                    SCM_invoke(test, 0));
}

SCM_DefineClosure(function2, );

SCM_DeclareFunction(function2) {
  thing2;
  return thing3;
}

SCM_DefineClosure(function3, );

SCM_DeclareFunction(function3) {
  return (thing0,
          SCM_invoke(SCM_close(SCM_CfunctionAddress(function1), 1, 0), 1,
                     SCM_close(SCM_CfunctionAddress(function2), 0, 0)));
}

/* Main */
int main(void) {
  SCM_print(SCM_invoke(SCM_close(SCM_CfunctionAddress(function3), 0, 0), 0));
  exit(0);
}

/* End of generated code. */
