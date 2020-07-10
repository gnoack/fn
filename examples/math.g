// This is a grammar which evaluates math expressions while parsing.
//
// Usage:
// (load-file "examples/grammar-utils.fn")
// (def math (load-grammar! "examples/math.g"))
// (def parse-math (read-and-check-empty-remainder (@@ math-grammar exprAdd)))
// (parse-math "2*(3+4)")
grammar math-grammar ((base-grammar DIGIT)) {
  exprAdd ::= exprMul:a "+" exprAdd:b      => (+ a b)
            | exprMul;
  exprMul ::= exprPri:a "*" exprMul:b      => (* a b)
            | exprPri;
  exprPri ::= "(" exprAdd:a ")"            => a
            | decimal;
  decimal ::= DIGIT+:ds                    => (string->int (list->string ds));
}
