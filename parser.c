// Auto-generated.
#include "parser.h"

#include "cons.h"
#include "value.h"

#include "strings.h"

oop parser_decls() { return LIST(LIST(make_symbol("def"), make_symbol("digit?"), LIST(make_symbol("lambda"), LIST(make_symbol("c")), LIST(make_symbol("_and"), LIST(make_symbol("<="), LIST(make_symbol("char->num"), make_char('0')), LIST(make_symbol("char->num"), make_symbol("c"))), LIST(make_symbol("<="), LIST(make_symbol("char->num"), make_symbol("c")), LIST(make_symbol("char->num"), make_char('9')))))), LIST(make_symbol("def"), make_symbol("read"), LIST(make_symbol("lambda"), LIST(make_symbol("in")), LIST(make_symbol("let"), LIST(LIST(make_symbol("c"), LIST(make_symbol("first"), make_symbol("in"))), LIST(make_symbol("cs"), LIST(make_symbol("rest"), make_symbol("in")))), LIST(make_symbol("if"), LIST(make_symbol("digit?"), make_symbol("c")), LIST(make_symbol("string->int"), make_symbol("in")), LIST(make_symbol("list"), make_char('a'), make_char('b'), make_char('c'))))))); }

