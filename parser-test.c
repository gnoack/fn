// Auto-generated.

#include "parser-test.h"

#include "cons.h"
#include "value.h"
#include "strings.h"

#include "tests.h"

oop parser_test_decls() { return LIST(LIST(make_symbol("eql"), make_smallint(123L), LIST(make_symbol("read"), LIST(make_symbol("list"), make_char('1'), make_char('2'), make_char('3')))), LIST(make_symbol("eql"), LIST(make_symbol("list"), make_char('a'), make_char('b'), make_char('c')), LIST(make_symbol("read"), LIST(make_symbol("list"), make_char('"'), make_char('a'), make_char('b'), make_char('c'), make_char('"'))))); }

void parser_tests() {
  run_lisp_tests(parser_test_decls());
}
