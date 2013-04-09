
// Lisp grammar.
grammar lisp-grammar ((base ANY EMPTY WHITESPACE)) {
  dblquote    ::= "\"";
  backslash   ::= "\\";
  semicolon   ::= ";";
  newline     ::= "\n";

  comment     ::= semicolon (~newline ANY)*;

  whitespace ::= comment | WHITESPACE;

  separator   ::= whitespace | "(" | ")" | EMPTY;

  anything-but-separator ::= ~separator ANY:e       => e;

  symbol      ::= ~"#" anything-but-separator+:x    => (string->symbol (list->string x));

  character   ::= "#" "\\" ( "Newline" => #\Newline
                           | "Space" => #\Space
                           | "Tab" => #\Tab
                           | ";" => #\;
                           | ANY ):c
                  ~anything-but-separator           => c;

  escapedchar ::= backslash ANY:e                   => e
                | ANY;
  stringchar  ::= ~dblquote escapedchar:c           => c;
  string      ::= dblquote stringchar*:cs dblquote  => (list->string cs);

  integer     ::= DIGIT+:ds                         => (string->int (list->string ds));

  exprs       ::= expr*:es whitespace*              => es;
  sexpression ::= "(" expr*:es whitespace* ")"      => es;

  prefix-expr ::= "\'" expr:e                       => (list (quote quote) e)
                | "`" expr:e                        => (list (quote backquote) e)
                | "`" "@" expr:e                    => (list (quote unquote-list) e)
                | "`" expr:e                        => (list (quote unquote) e);

  expr        ::= whitespace* ( prefix-expr
                              | sexpression
                              | integer
                              | string
                              | character
                              | symbol ):e          => e;

  // Also consumes trailing whitespaces and comments.
  expr_whitespace ::= expr:e whitespace*            => e;
}
