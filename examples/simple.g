
grammar example-grammar ((base ANY)) {
    my_sep           ::= ",";
    my_item          ::= "a" | "b" | "c";
    listof sep item  ::= item:i (sep item:i => i)*:is           => (cons i is);

    example          ::= listof(my_sep, my_item);
}
