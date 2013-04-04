// Auto-generated.

#include "modules-test.h"

#include "cons.h"
#include "value.h"
#include "strings.h"

#include "tests.h"

oop modules_test_decls() { return LIST(LIST(make_symbol("->true"), LIST(make_symbol("def"), make_symbol("test-module"), LIST(make_symbol("let"), LIST(LIST(make_symbol("a"), make_smallint(1L)), LIST(make_symbol("b"), make_smallint(2L)), LIST(make_symbol("c"), make_smallint(3L))), LIST(make_symbol("make-module"), make_symbol("a"), make_symbol("b"))))), LIST(make_symbol("eq?"), make_smallint(1L), LIST(make_symbol("dict-get"), make_symbol("test-module"), LIST(make_symbol("quote"), make_symbol("a")))), LIST(make_symbol("eq?"), make_smallint(2L), LIST(make_symbol("dict-get"), make_symbol("test-module"), LIST(make_symbol("quote"), make_symbol("b")))), LIST(make_symbol("eq?"), make_smallint(2L), LIST(make_symbol("dict-size"), make_symbol("test-module"))), LIST(make_symbol("import"), LIST(LIST(make_symbol("test-module"), LIST(make_symbol("test/a"), make_symbol("a")))), LIST(make_symbol("="), make_symbol("test/a"), make_smallint(1L))), LIST(make_symbol("import"), LIST(LIST(make_symbol("test-module"), LIST(make_symbol("test/a"), make_symbol("a")), LIST(make_symbol("test/b"), make_symbol("b")))), LIST(make_symbol("and"), LIST(make_symbol("="), make_symbol("test/a"), make_smallint(1L)), LIST(make_symbol("="), make_symbol("test/b"), make_smallint(2L)))), LIST(make_symbol("import"), LIST(LIST(make_symbol("test-module"), make_symbol("a"), make_symbol("b"))), LIST(make_symbol("and"), LIST(make_symbol("="), make_symbol("a"), make_smallint(1L)), LIST(make_symbol("="), make_symbol("b"), make_smallint(2L)))), LIST(make_symbol("->true"), LIST(make_symbol("def"), make_symbol("other-test-module"), LIST(make_symbol("let"), LIST(LIST(make_symbol("x"), make_smallint(10L)), LIST(make_symbol("y"), make_smallint(20L)), LIST(make_symbol("z"), make_smallint(30L))), LIST(make_symbol("make-module"), make_symbol("x"), make_symbol("y"), make_symbol("z"))))), LIST(make_symbol("import"), LIST(LIST(make_symbol("test-module"), LIST(make_symbol("a"), make_symbol("a")), LIST(make_symbol("b"), make_symbol("b"))), LIST(make_symbol("other-test-module"), LIST(make_symbol("x"), make_symbol("x")), LIST(make_symbol("y"), make_symbol("y")), LIST(make_symbol("z"), make_symbol("z")))), LIST(make_symbol("eq?"), make_smallint(63L), LIST(make_symbol("+"), make_symbol("a"), make_symbol("b"), make_symbol("x"), make_symbol("y"), make_symbol("z"))))); }

void modules_tests() {
  run_lisp_tests(modules_test_decls());
}
