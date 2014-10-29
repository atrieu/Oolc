  type codeArea =
    {
      latitudeLo : float;
      longitudeLo : float;
      latitudeHi : float;
      longitudeHi : float;
      latitudeCenter : float;
      longitudeCenter : float;
      codeLength : int
    }
  exception Not_valid
  exception Invalid_Open_Location_Code_length
  val isValid : string -> bool
  val isShort : string -> bool
  val isFull : string -> bool
  val encode : ?codeLength : int -> float -> float -> string
  val decode : string -> codeArea
  val shortenBy4 : string -> float -> float -> string
  val shortenBy6 : string -> float -> float -> string
  val recoverNearest : string -> float -> float -> string
