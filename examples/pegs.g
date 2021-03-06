// Self-describing Parsing Expression Grammar syntax.
//
// This is used to bootstrap the runtime/pegs-parser.fn file.
//
// A grammar can import rules from other grammars at the top.
// In this example, we import rules like ALPHA and DIGIT from the
// "base" grammar.
//
// Rule Syntax:
//   rulename ::= PEXPR => LEXPR
// where
//   PEXPR  is a parsing expression and may define variables along the way
//   LEXPR  is a Lisp expression using these variables and producing the result
//
// Common PEG operations:
//   P        ; the PEG itself
//   P Q      ; P, then Q
//   P | Q    ; P or Q
//   P+       ; P once or more
//   P*       ; P 0 or more times
//   P:x      ; name the result of P as `x'
//   ~P       ; negative lookahead (positive lookahead: ~~P)
//
grammar pegs-grammar ((base-grammar ALPHA DIGIT ANY WHITESPACE EPSILON)
                      (lisp-grammar (LISP-STRING string) (LISP-EXPR expr))) {
  whitespace      ::= WHITESPACE | "//" (~"\n" ANY)+;

  token peg                  ::= whitespace* peg:t whitespace*  => t;
  listof item sep            ::= item:a (sep item:it => it)*:as => (cons a as);
  listof_or_nothing item sep ::= listof(item,sep) | EPSILON;

  openbracket     ::= token("(");
  closebracket    ::= token(")");

  symbol_start    ::= ALPHA | "_";
  symbol_more     ::= symbol_start | "-" | DIGIT;
  symbol          ::= symbol_start:c symbol_more*:cs    => (string->symbol
                                                             (list->string
                                                               (cons c cs)));

  string_expr     ::= whitespace* LISP-STRING:s         => (make-peg-seq-expr
                                                             (map (lambda (ch) `(peg= ,ch))
                                                                  (string->list s)));
  symbol_expr     ::= whitespace* "\'" symbol:s         => `(peg= (quote ,s));
  inv_args        ::= "(" listof_or_nothing(expr1, token(",")):as
                      ")" whitespace*                   => as
                    | EPSILON                           => (list);
  invocation_expr ::= whitespace* symbol:s inv_args:as  => `(peg-indirect ,s ,@as);
  bracketed_expr  ::= openbracket expr1:e closebracket  => e;
  expr4           ::= negated_expr
                    | string_expr
                    | symbol_expr
                    | invocation_expr
                    | bracketed_expr;
  negated_expr    ::= "~" expr4:e                       => `(peg-not ,e);
  plus_expr       ::= expr4:e "+"                       => `(peg+ ,e);
  star_expr       ::= expr4:e "*"                       => `(peg* ,e);
  expr3           ::= plus_expr | star_expr | expr4;

  bindingvar      ::= ":" symbol:s                      => s
                    | EPSILON                           => (quote _);

  binding         ::= expr3:e bindingvar:v              => (list v e);
  action_sequence ::= listof(binding, whitespace*):bs
                      token("=>") LISP-EXPR:action      => `(peg-let ,bs ,action);
  sequence        ::= token(expr3)+:es                  => (make-peg-seq-expr es);
  expr2           ::= action_sequence | sequence;
  expr1           ::= listof(expr2, token("|")):es      => (make-peg-alt-expr es);
  rule            ::= whitespace* symbol:n token(symbol)*:ps token("::=") expr1:e token(";")
                                                        => `(defrule ,n ,ps ,e);
  imports         ::= openbracket LISP-EXPR*:es closebracket  => es
                    | EPSILON;
  grammar         ::= whitespace* token("grammar") symbol:n imports:im
                      token("{") rule*:rs token("}")     => `(defgrammar ,n ,im ,@rs);
}
