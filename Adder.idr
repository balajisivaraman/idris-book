module Adder

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num num => (numargs : Nat) -> (acc : num) -> AdderType numargs num
adder Z acc = acc
adder (S k) acc = \i => adder k (i + acc)
