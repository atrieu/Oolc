# Oolc

Oolc is an OCaml implementation of [Open Location Code](https://github.com/google/open-location-code).

Oolc is a library providing the following functions:

    isValid : string -> bool
    isShort : string -> bool
    isFull : string -> bool
    encode : ?codeLength : int -> float -> float -> string
    decode : string -> codeArea
    shortenBy4 : string -> float -> float -> string
    shortenBy6 : string -> float -> float -> string
    recoverNearest : string -> float -> float -> string

## Dependencies
Oolc is written in OCaml and requires OCaml >= 4.01.0 and `ocamlfind` to install.

## Installation
* To install with [OPAM](http://opam.ocamlpro.com/) (recommended):

      opam install oolc

* To get the development version and compile:

      git clone git@github.com:atrieu/Oolc.git
      cd oolc
      ocaml setup.ml -configure
      ocaml setup.ml -build
      ocaml setup.ml -install
