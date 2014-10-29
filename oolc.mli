module Oolc : sig
  type codeArea
  exception Not_valid
  exception Invalid_Open_Location_Code_length
  val isValid : string -> bool
  val isShort : string -> bool
  val isFull : string -> bool
  val encode : ?codeLength : int -> float -> float -> string
  val decode : string -> codeArea
end
