
// TODO: Symbols.
grammar smalltalk-grammar ((base ALPHA DIGIT ANY END-OF-INPUT WHITESPACE EPSILON)) {
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
  literal-symbol      ::= "#" (ALPHA | bin-op | ":")+:cs => `(quote ,(string->symbol (list->string cs)));
  literal-character   ::= "$" ALPHA:a                    => a;  // TODO: Escape codes!

  unary-message-send  ::= expr4:e WORD:sel               => `(msg-send ,e (quote ,sel)); // TODO: Support "a b c" (chained unary sends)
  binary-message-send ::= expr3:e1 bin-op:sel expr3:e2   => `(msg-send ,e1 (quote ,sel) ,e2);
  nary-message-send   ::= expr2:e kwordlist(expr2):l     => `(msg-send ,e (quote ,(first l)) ,@(second l));
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
  comment     ::= "\"" stringchar*:cs "\""          => `(st-comment ,(list->string cs));

  // --------- Bodies
  listof item sep  ::= item:a (sep item:it => it)*:as => (cons a as);
  statements  ::= listof(statement, tk("."));
  statement   ::= tk("^") expr:e                    => `(st-return ,e)
                | var-name:v tk(":=") expr:e        => `(set! ,v ,e)
                | expr:e                            => e;
  body        ::= var-decl:vs statements:ss         => `(st-body ,vs ,@ss)
                | statements:ss                     => `(st-body () ,@ss);
  var-decl    ::= tk("|") var-name+:vs tk("|")      => vs;

  // --------- Method definitions
  type-name   ::= WORD:t                            => (string->symbol (string-append "@" (symbol->string t)));
  method-sig  ::= WORD:sel                          => `(,sel ())
                | bin-op:sel var-name:v             => `(,sel (,v))
                | kwordlist(var-name):sig           => sig;
  method-body ::= tk("[") body:b tk("]")            => b;
  method-def  ::= type-name:v tk(">>") method-sig:s method-body:b  => `(st-defm ,v ,@s ,b);

  // ---- Blocks
  block-expr    ::= tk("[") block-arglist:as statements:ss tk("]")  => `(lambda ,as ,@ss);
  block-arglist ::= ARG*:args tk("|")               => args
                  | EPSILON                         => '();

  // ---- File
  file        ::= method-def*;
}
