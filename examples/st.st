
Cons>>car [ Smalltalk object: self at: 1 ]
Cons>>cdr [ Smalltalk object: self at: 2 ]

Cons>>each: aBlock [
  aBlock value: self car.
  self cdr each: aBlock
]
Nil>>each: aBlock [ ^ self ]

Cons>>map: aBlock [
  Cons car: (aBlock value: self car)
       cdr: (self cdr map: aBlock)
]
Nil>>map: aBlock [ ^ nil ]

Cons>>filter: aPredicate [
  (aPredicate value: self car) ifTrue: [
    ^ Cons car: self car
           cdr: (self cdr filter: aPredicate)
  ] ifFalse: [
    ^ self cdr filter: aPredicate
  ]
]
Nil>>filter: aPredicate [ ^ nil ]

Nil>>isNil [ ^ true ]
Object>>isNil [ ^ false ]

Type>>methodTable [ Smalltalk object: self at: 1 ]
Type>>superclass [ Smalltalk object: self at: 3 ]
Type>>installMethod: method at: selector [
  self methodTable put: method at: selector
]


Symbol>>value: receiver [ receiver send: self ]

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

True>>value [ self ]
False>>value [ self ]

True>>ifTrue: aBlock [ aBlock value ]
False>>ifTrue: aBlock [ ^ false ]
True>>ifFalse: aBlock [ ^ false ]
False>>ifFalse: aBlock [ aBlock value ]
True>>ifTrue: aBlock ifFalse: anotherBlock [ aBlock value ]
False>>ifTrue: aBlock ifFalse: anotherBlock [ anotherBlock value ]

True>>or: other [ ^ true ]
False>>or: other [ ^ other value ]
True>>and: other [ ^ other value ]
False>>and: other [ ^ false ]

Object>>unknownMessage: message arguments: arguments [
  ('Message ' ++ message asString ++ ' not understood.') println.
  Smalltalk raise: #MessageNotUnderstood
]

Object>>methods [
  ^ self class methodTable keys
]

CompiledProcedure>>repeatUntil: aBlock [
  self value.
  aBlock value ifFalse: [ self repeatUntil: aBlock ]
]

Type>>asString [
  ^ Smalltalk object: self at: 2
]

Object>>asString [
  ^ 'a ' ++ (self class) asString
]

Smallint>>asString [ ^ Smalltalk prettyprint: self ]
String>>asString [ ^ Smalltalk prettyprint: self ]
Dict>>asString [ ^ Smalltalk prettyprint: self ]
Nil>>asString [ ^ 'nil' ]
True>>asString [ ^ 'true' ]
False>>asString [ ^ 'false' ]
Object>>isCons [ ^ false ]
Cons>>isCons [ ^ true ]
Cons>>asStringInner [
  self cdr isNil ifTrue: [
    self car asString
  ] ifFalse: [
    self cdr isCons ifTrue: [
      self car asString ++ ' ' ++ self cdr asStringInner
    ] ifFalse: [
      self car asString ++ ' . ' ++ self cdr asString
    ]
  ]
]
Cons>>asString [ '(' ++ self asStringInner ++ ')' ]

Smallint>>upto: other [
  self < other ifTrue: [
    ^ Cons car: self cdr: (self + 1 upto: other)
  ] ifFalse: [
    ^ nil
  ]
]
Smallint>>isEven [ ^ (self % 2) = 0 ]
Smallint>>isOdd [ ^ (self % 2) = 1 ]

String>>isEmpty [ self length = 0 ]
String>>join: parts [
  | result |
  result := ''.
  parts each: [ :part |
    result isEmpty ifFalse: [
      result := result ++ self
    ].
    result := result ++ part
  ].
  ^ result
]

Main>>repl [
  | line result |
  [
    [
      line := RL readline: 'ST> '.
      line isNil ifFalse: [
        RL addToHistory: line.
        (Smalltalk eval: line) asString println
      ]
    ] onError: [ :reason |
      '**** An error happened!' println.
      reason println.
      'Please try again.' println.
      line := 'try again'
    ]
  ] repeatUntil: [ line isNil or: [ line isEmpty ] ].
  'Goodbye!' println
]

Readline>>memory: memory [
  Smalltalk object: self put: memory at: 1
]

Readline>>memory [ Smalltalk object: self at: 1 ]

Readline>>complete: text state: state globalEnv: env [
  | memory prefix |
  0 = state ifTrue: [
    prefix := '#' ++ text.
    self memory: ((env keys map: #asString) filter: [ :a |
      a startsWith: text
    ])
  ].
  memory := self memory.
  memory isCons ifTrue: [
    self memory: memory cdr.
    ^ memory car
  ] ifFalse: [
    ^ nil
  ]
]

Main>>main [
  | name |

  'Arguments:' println.
  self args each: [ :arg |
    (' arg: ' ++ arg) println
  ].
  'Methods available on cons cells:' println.
  (Cons car: 1 cdr: 2) class methodTable eachKey: #println.

  'Entering repl...' println.
  self repl.
  ^ 0
]
