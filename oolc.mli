(** An OCaml Implementation of Open Location Code. *)

(** Representation of an area indicated by a code. *)
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

(** Exception raised when asking to encode a location with too
    small a code length. *)
exception Invalid_Open_Location_Code_length

(** [isValid s] returns a boolean indicating whether the string
    is a valid Open Location Code sequence or not.*)
val isValid : string -> bool

(** [isShort s] returns a boolean indicating whether the string
    is a valid short Open Location Code sequence or not.*)
val isShort : string -> bool

(** [isFull s] returns a boolean indicating whether the string
    is a valid full Open Location Code sequence or not.*)
val isFull : string -> bool

(** [encode latitude longitude] returns an Open Location code
    corresponding to the location given. An optional length can
    be given, if not specified, a code with 10 characters will be
    generated. If the length asked is too small,
    [Invalid_Open_Location_Code-length] is raised. *)
val encode : ?codeLength : int -> float -> float -> string

(** [decode c] returns the coordinates corresponding to the [c],
    if the code is not valid, an exception is raised. *)
val decode : string -> codeArea

(** [shortenBy4 c latitude longitude] tries to remove the first four
    characters of [c] if it is a valid code. This will only succeed
    if both the latitude and longitude are less than 0.25 degrees
    from the code center. If not, the original code is returned. *)
val shortenBy4 : string -> float -> float -> string

(** [shortenBy6 c latitude longitude] tries to remove the first six
    characters of [c] if it is a valid code. This will only succeed
    if both the latitude and longitude are less than 0.0125 degrees
    from the code center. If not, the original code is returned. *)
val shortenBy6 : string -> float -> float -> string
(** [recoverNearest c latitude longitude] returns the nearest full
    Open Location Code matching with the short code [c] and the
    location provided by [latitude] and [longitude]. *)
val recoverNearest : string -> float -> float -> string
