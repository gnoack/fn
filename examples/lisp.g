
// Lisp grammar.
grammar lisp-grammar ((base ANY END-OF-INPUT WHITESPACE)) {
  comment     ::= ";" (~"\n" ANY)*;
  whitespace  ::= WHITESPACE | comment;

  separator   ::= whitespace | "(" | ")" | END-OF-INPUT;

  anything-but-separator ::= ~separator ANY:e       => e;

  symbol      ::= ~"#" anything-but-separator+:x    => (string->symbol (list->string x));

  character   ::= "#" "\\" ( "Newline" => #\Newline
                           | "Space" => #\Space
                           | "Tab" => #\Tab
                           | ";" => #\;
                           | ANY ):c
                  ~anything-but-separator           => c;

  escapedchar ::= "\\" ANY:e                        => e
                | ANY;
  stringchar  ::= ~"\"" escapedchar:c               => c;
  string      ::= "\"" stringchar*:cs "\""          => (list->string cs);

  integer     ::= DIGIT+:ds                         => (string->int (list->string ds));

  sexpression ::= "(" expr*:es whitespace* ")"      => es;

  prefix-expr ::= "\'" expr:e                       => (list (quote quote) e)
                | "`" expr:e                        => (list (quote backquote) e)
                | "`" "@" expr:e                    => (list (quote unquote-list) e)
                | "," expr:e                        => (list (quote unquote) e);

  expr        ::= whitespace* ( prefix-expr
                              | sexpression
                              | integer
                              | string
                              | character
                              | symbol ):e          => e;

  // Also consumes trailing whitespaces and comments.
  expr_whitespace ::= expr:e whitespace*            => e;
  exprs           ::= expr*:es whitespace*          => es;
}
