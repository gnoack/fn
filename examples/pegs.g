
// TODO: This is a work in progress.
// TODO: Implement high-level rules.
grammar pegs-grammar ((base ALPHA ANY WHITESPACE EPSILON)
                      (lisp (LISP-STRING string) (LISP-EXPR expr))) {
  whitespace      ::= WHITESPACE | "//" (~"\n" ANY)+;

  token(peg)      ::= whitespace* peg whitespace*;
  listof+(item,sep) ::= item:a (sep item:it => it)+:as => (cons a as);
  listof*(item,sep) ::= listof+(item,sep) | EPSILON;

  singlequote     ::= "\'";
  colon           ::= ":";

  semicolon       ::= token(";");
  arrow           ::= token("=>");
  define          ::= token("::=");
  grammar_kw      ::= token("grammar");
  opencurly       ::= token("{");
  closecurly      ::= token("}");
  openbracket     ::= token("(");
  closebracket    ::= token(")");

  symbol          ::= (ALPHA | "-" | "_")+:cs           => (string->symbol
                                                             (list->string cs));
  string_expr     ::= whitespace* LISP-STRING:s         => (make-peg-seq-expr
                                                             (map (lambda (ch) `(peg= ,ch))
                                                                  (string->list s)));
  symbol_expr     ::= whitespace* singlequote symbol:s  => `(peg= (quote ,s));
  invocation_expr ::= whitespace* symbol:s              => `(peg-indirect ,s);
  bracketed_expr  ::= openbracket expr:e closebracket   => e;

  negated_expr    ::= "~" expr4:e                       => e;
  expr4           ::= negated_expr
                    | string_expr
                    | symbol_expr
                    | invocation_expr
                    | bracketed_expr;
  plus_expr       ::= expr4:e "+"                       => e;
  star_expr       ::= expr4:e "*"                       => e;
  expr3           ::= plus_expr | star_expr | expr4;

  bindingvar      ::= colon symbol:s                    => s
                    | EPSILON                           => (quote _);

  binding         ::= expr3:e bindinvar:v               => (list v e);
  action_sequence ::= listof*(binding, whitespace*):bs
                      arrow LISP-EXPR:action            => `(peg-let ,bs ,action);
  sequence        ::= token(expr3)*:es                  => (make-peg-seq-expr es);
  expr2           ::= action_sequence | sequence;
  expr1           ::= listof+(expr2, token("|")):es     => (make-peg-alt-expr es);
  rule            ::= whitespace* symbol:n define expr1:e semicolon
                                                        => `(defrule ,n ,e);
  imports         ::= openbracket LISP-EXPR*:es closebracket  => es
                    | EPSILON;
  grammar         ::= whitespace* grammar_kw symbol:n imports:im
                      opencurly rule*:rs closecurly     => `(defgrammar ,n ,im ,@rs);
}
