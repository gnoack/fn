
grammar smalltalk-grammar ((base-grammar ALPHA DIGIT ANY END-OF-INPUT WHITESPACE EPSILON)) {
  // foo: bar: baz: interleaved with R (e.g. expressions or parameter names)
  kwordlist R         ::= (WORDK R)+:xs  =>
                               `(,(string->symbol (apply string-append (map first xs)))
                                 ,(map second xs));

  tk P                ::= WHITESPACE* P:t WHITESPACE* => t;

  expr                ::= expr1;
  expr1               ::= nary-message-send | expr2;
  expr2               ::= binary-message-send | expr3;
  expr3               ::= unary-message-send | expr4;
  expr4               ::= var-name | expr5;
  expr5               ::= literal-expr | expr6;
  expr6               ::= block-expr | expr7;
  expr7               ::= tk("(") expr:e tk(")") => e;

  literal-expr        ::= tk(literal-number) |
                          tk(literal-character) |
                          tk(string) |
                          tk(literal-symbol);

  var-name            ::= WORD;
  literal-number      ::= DIGIT+:ds                      => (string->int (list->string ds));
  literal-symbol      ::= "#" (ALPHA | BINCHAR | ":")+:cs => `(quote ,(string->symbol (list->string cs)));
  literal-character   ::= "$" ALPHA:a                    => a;  // TODO: Escape codes!

  unary-message-send  ::= expr4:e WORD+:sels             => (reduce (lambda (e sel) `(send ,e (quote ,sel))) sels e);
  binary-message-send ::= expr3:e (bin-op:sel expr3:arg => (list sel arg))+:sends
                              => (reduce (lambda (e send)
                                           `(send ,e (quote ,(first send)) ,(second send)))
                                         sends e);
  nary-message-send   ::= expr2:e kwordlist(expr2):l     => `(send ,e (quote ,(first l)) ,@(second l));
  bin-op              ::= BINCHAR+:cs  => (string->symbol (list->string cs));

  // Tokens.
  WORD                ::= tk(ALPHA+:as ~":" => as):as    => (string->symbol (list->string as));
  WORDK               ::= tk(ALPHA+:as ":" => as):as     => (list->string (append as (list #\:)));  // String!
  ARG                 ::= tk(":" ALPHA+:as => as):as     => (string->symbol (list->string as));
  BINCHAR             ::= ( "+" | "*" | "-" | "/" | "%" | "~" | "=" | "^" | "<" | ">" | "@" );

  // Strings and comments.
  escapedchar ::= "\\n"                             => #\Newline
                | "\\t"                             => #\Tab
                | "\\" ANY:e                        => e
                | ANY;
  stringchar  ::= ~"\'" escapedchar:c               => c;
  string      ::= "\'" stringchar*:cs "\'"          => (list->string cs);

  commentchar ::= ~"\"" escapedchar:c               => c;
  comment     ::= "\"" commentchar*:cs "\""         => `(st-comment ,(list->string cs));

  // --------- Bodies
  listof item sep  ::= item:a (sep item:it => it)*:as => (cons a as);
  statements  ::= listof(statement, tk("."));
  statement   ::= tk("^") expr:e                    => `(st-return ,e)
                | comment:e                         => e
                | var-name:v tk(":=") expr:e        => `(set! ,v ,e)
                | expr:e                            => e;
  body        ::= var-decl:vs statements:ss         => `(st-body ,vs ,@ss)
                | statements:ss                     => `(st-body () ,@ss);
  var-decl    ::= tk("|") var-name+:vs tk("|")      => vs;

  // --------- Method definitions
  type-name   ::= WORD:t                            => t;
  type-ref    ::= type-name:n tk("class")           => `(type-of ,n)
                | type-name:n                       => n;
  method-sig  ::= WORD:sel                          => `(,sel ())
                | bin-op:sel var-name:v             => `(,sel (,v))
                | kwordlist(var-name):sig           => sig;
  method-body ::= tk("[") body:b tk("]")            => b;
  method-def  ::= type-ref:v tk(">>") method-sig:s method-body:b  => `(st-defm ,v ,@s ,b);

  // --------- Top level expressions
  do-block    ::= tk("do:") tk("[") body:b tk("]")  => b;
  top-level-expr ::= method-def | do-block;

  // ---- Blocks
  block-expr    ::= tk("[") block-arglist:as statements:ss tk("]")  => `(lambda ,as ,@ss);
  block-arglist ::= ARG*:args tk("|")               => args
                  | EPSILON                         => (list);

  // ---- File
  file        ::= top-level-expr*;
}
