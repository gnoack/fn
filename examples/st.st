
Cons>>car [ Smalltalk object: self at: 1 ]
Cons>>cdr [ Smalltalk object: self at: 2 ]

Cons>>each: aBlock [
  aBlock value: self car.
  self cdr each: aBlock
]
Nil>>each: aBlock [ ^ self ]

Cons>>map: aBlock [
  Smalltalk cons: (aBlock value: self car)
             and: (self cdr map: aBlock)
]
Nil>>map: aBlock [ ^ nil ]

Type>>methodTable [ Smalltalk object: self at: 1 ]

Symbol>>value: receiver [ Smalltalk send: self to: receiver ]
Symbol>>println [ (self asString) println ]

Main>>args [ Smalltalk object: self at: 1 ]

Dict>>keys [ self keyValuePairs map: #car ]
Dict>>values [ self keyValuePairs map: #cdr ]
Dict>>each: aBlock [
  self keyValuePairs each: [ :kv |
    aBlock value: (kv car) value: (kv cdr)
  ].
  ^ self
]
Dict>>eachKey: aBlock [ self keys each: aBlock ]

Dict>>println [
  self keys each: [ :name |
    (' * ' ++ name asString) println
  ].
  ^ self
]

True>>value [ self ]
False>>value [ self ]

True>>ifFalse: aBlock [ ^ self ]
False>>ifFalse: aBlock [ aBlock value ]

CompiledProcedure>>repeatUntil: aBlock [
  self value.
  aBlock value ifFalse: [ self repeatUntil: aBlock ]
]

String>>isEmpty [ self length = 0 ]

Main>>repl [
  | line result |
  [
    [
      line := Smalltalk readline: 'ST> '.
      result := Smalltalk eval: line.
      Smalltalk println: result
    ] onError: [ :reason |
      Smalltalk println: '**** An error happened!'.
      Smalltalk println: reason.
      Smalltalk println: 'Please try again.'.
      line := 'try again'
    ]
  ] repeatUntil: [ line isEmpty ]
]

Main>>main [
  | name |

  'Arguments:' println.
  self args each: [ :arg |
    (' arg: ' ++ arg) println
  ].
  'Methods available on cons cells:' println.
  (Smalltalk typeOf: (Smalltalk cons: 1 and: 2)) methodTable eachKey: #println.

  'Entering repl...' println.
  self repl.
  ^ 0
]
