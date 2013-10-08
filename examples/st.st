
cons>>car [ Smalltalk object: self at: 1 ]
cons>>cdr [ Smalltalk object: self at: 2 ]

cons>>each: aBlock [
  aBlock value: self car.
  self cdr each: aBlock
]
nil>>each: aBlock [ ^ self ]

cons>>map: aBlock [
  Smalltalk cons: (aBlock value: self car)
             and: (self cdr map: aBlock)
]
nil>>map: aBlock [ ^ nil ]

type>>methodTable [ Smalltalk object: self at: 1 ]

symbol>>value: receiver [ Smalltalk send: self to: receiver ]
symbol>>println [ (self asString) println ]

Main>>args [ Smalltalk object: self at: 1 ]

dict>>keys [ self keyValuePairs map: #car ]
dict>>values [ self keyValuePairs map: #cdr ]
dict>>each: aBlock [
  self keyValuePairs each: [ :kv |
    aBlock value: (kv car) value: (kv cdr)
  ].
  ^ self
]
dict>>eachKey: aBlock [ self keys each: aBlock ]

dict>>println [
  self keys each: [ :name |
    (' * ' ++ name asString) println
  ].
  ^ self
]

Main>>main [
  'Arguments:' println.
  self args each: [ :arg |
    (' arg: ' ++ arg) println
  ].
  'Methods available on cons cells:' println.
  (Smalltalk typeOf: (Smalltalk cons: 1 and: 2)) methodTable eachKey: #println.
  ^ 0
]
