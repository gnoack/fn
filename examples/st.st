
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

Nil>>isNil [ ^ true ]
Object>>isNil [ ^ false ]

Type>>methodTable [ Smalltalk object: self at: 1 ]
Type>>superType [ Smalltalk object: self at: 3 ]
Type>>superType: type [ Smalltalk object: self put: type at: 3 ]
Type>>type: type [ "TODO: Remove this.". Smalltalk object: self put: type at: 0 ]
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

True>>or: other [ ^ true ]
False>>or: other [ ^ other value ]
True>>and: other [ ^ other value ]
False>>and: other [ ^ false ]

Object>>unknownMessage: message arguments: arguments [
  ('Message ' ++ message asString ++ ' not understood.') println.
  Smalltalk raise: #MessageNotUnderstood
]

Object>>methods [
  ^ self type methodTable keys
]

CompiledProcedure>>repeatUntil: aBlock [
  self value.
  aBlock value ifFalse: [ self repeatUntil: aBlock ]
]

Type>>asString [
  ^ Smalltalk object: self at: 2
]

Object>>asString [
  ^ 'a ' ++ (self type) asString
]

Smallint>>asString [ ^ Smalltalk prettyprint: self ]
String>>asString [ ^ Smalltalk prettyprint: self ]
Dict>>asString [ ^ Smalltalk prettyprint: self ]
Nil>>asString [ ^ 'nil' ]
True>>asString [ ^ 'true' ]
False>>asString [ ^ 'false' ]

String>>isEmpty [ self length = 0 ]

Main>>repl [
  | line result |
  [
    [
      line := Smalltalk readline: 'ST> '.
      line isNil ifFalse: [
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

Main>>main [
  | name |

  'Arguments:' println.
  self args each: [ :arg |
    (' arg: ' ++ arg) println
  ].
  'Methods available on cons cells:' println.
  (Smalltalk cons: 1 and: 2) type methodTable eachKey: #println.

  'Entering repl...' println.
  self repl.
  ^ 0
]
