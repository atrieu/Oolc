module Oolc = struct

  let code_alphabet_ = "23456789CFGHJMPQRVWX"
  let encoding_base_ = String.length code_alphabet_
  let latitude_max_ = 90.
  let longitude_max_ = 180.
  let prefix_ = '+'
  let separator_ = '.'
  let separator_position_ = 4
  let min_short_code_len_ = 4
  let max_short_code_len_ = 7
  let pair_code_length_ = 10
  let grid_rows_ = 5.
  let pair_resolutions_ = [| 20.0; 1.0; 0.05; 0.0025; 0.000125 |]
  let grid_columns_ = 4.
  let grid_size_degrees_ = 0.000125
  exception Not_valid
  exception Invalid_Open_Location_Code_length
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

  let create_codeArea latitudeLo longitudeLo latitudeHi longitudeHi codeLength =
    { latitudeLo; longitudeLo; latitudeHi; longitudeHi; codeLength;
      latitudeCenter = min latitude_max_ (latitudeLo +. (latitudeHi -. latitudeLo) /. 2.);
      longitudeCenter = min longitude_max_ (longitudeLo +. (longitudeHi -. longitudeLo) /. 2.)
    }

  let isValid (code : string) : bool =
    try
      if String.length code = 0 then
        raise Not_valid;
      (* Checks if code contains the prefix character *)
      if String.contains code prefix_ then
        begin
          (* If the code contains more than one prefix character, it is not valid *)
          if String.index code prefix_ <> String.rindex code prefix_ then
            raise Not_valid;
          (* If the code includes the prefix character but not in the first position, it is not valid *)
          if String.index code prefix_ > 0 then
            raise Not_valid;
        end;
      (* Checks if code contains the separator character *)
      if String.contains code separator_ then
        begin
          (* If the code includes more than one separator, it is not valid *)
          if String.index code separator_ <> String.rindex code separator_ then
          raise Not_valid;
          if String.contains code prefix_ then
            begin
              let code = String.sub code 1 (String.length code - 1) in
              (* If its position is different from separator_position_, it is not valid *)
              if String.index code separator_ <> separator_position_ then
              raise Not_valid;
            end
          else
            (* If its position is different from separator_position_, it is not valid *)
            if String.index code separator_ <> separator_position_ then
              raise Not_valid;
        end;
      (* Checks the code contains only valid characters *)
      for i = 0 to String.length code - 1 do
        let character = code.[i] |> Char.uppercase in
        if character <> separator_ && character <> prefix_ && not (String.contains code_alphabet_ character) then
          raise Not_valid
      done;
      true
    with
      Not_valid -> false

  let isShort (code : string) : bool =
    try
      if not (isValid code) then
        raise Not_valid;
      if String.contains code separator_ then
        raise Not_valid;
      if String.contains code prefix_ then
        begin
          let code = String.sub code 1 (String.length code - 1) in
          if String.length code < min_short_code_len_ then
            raise Not_valid;
          if String.length code > max_short_code_len_ then
            raise Not_valid;
        end
      else
        begin
          if String.length code < min_short_code_len_ then
            raise Not_valid;
          if String.length code > max_short_code_len_ then
            raise Not_valid;
        end;
      true
    with
      Not_valid -> false

  let isFull (code : string) : bool =
    try
      if not (isValid code) then
        raise Not_valid;
      if String.contains code prefix_ then
        begin
          let code = String.sub code 1 (String.length code - 1) in
          let firstLatValue = String.index code_alphabet_ (Char.uppercase code.[0]) * encoding_base_ in
          if firstLatValue >= int_of_float latitude_max_ * 2 then
            raise Not_valid;
          if String.length code > 1 then
            let firstLngValue = String.index code_alphabet_ (Char.uppercase code.[1]) * encoding_base_ in
            if firstLngValue >= int_of_float longitude_max_ * 2 then
              raise Not_valid;
        end
      else
        begin
          let firstLatValue = String.index code_alphabet_ (Char.uppercase code.[0]) * encoding_base_ in
          if firstLatValue >= int_of_float latitude_max_ * 2 then
            raise Not_valid;
          if String.length code > 1 then
            let firstLngValue = String.index code_alphabet_ (Char.uppercase code.[1]) * encoding_base_ in
            if firstLngValue >= int_of_float longitude_max_ * 2 then
              raise Not_valid;
        end;
      true
    with
     Not_valid -> false

  let clipLatitude (latitude : float) : float =
    (max (-.90.) latitude) |> min 90.

  let normalizeLongitude (longitude : float) : float =
    let longitude = ref longitude in
    while (!longitude < (-.180.)) do
      longitude := !longitude +. 360.
    done;
    while (!longitude >= 180.) do
      longitude := !longitude -. 360.
    done;
    !longitude

  let computeLatitudePrecision (codeLength : int) : float =
    if codeLength <= 10 then
      20. ** (floor ((float_of_int codeLength) /. (-. 2.) +. 2.))
    else
      (20. ** (-.3.)) /. (grid_rows_ ** (float_of_int codeLength -. 10.))

  let encodePairs (latitude : float) (longitude : float) (codeLength : int) : string =
    let code = ref "" in
    let adjustedLatitude = ref (latitude +. latitude_max_) in
    let adjustedLongitude = ref (longitude +. longitude_max_) in
    let digitCount = ref 0 in
    while !digitCount < codeLength do
      let placeValue = pair_resolutions_.(!digitCount / 2) in
      let digitValue = floor (!adjustedLatitude /. placeValue) in
      adjustedLatitude := !adjustedLatitude -. (digitValue *. placeValue);
      code := !code ^ String.make 1 code_alphabet_.[int_of_float digitValue];
      incr digitCount;
      if !digitCount = codeLength then
        ()
      else
        begin
          let digitValue = floor (!adjustedLongitude /. placeValue) in
          adjustedLongitude := !adjustedLongitude -. (digitValue *. placeValue);
          code := !code ^ String.make 1 code_alphabet_.[int_of_float digitValue];
          incr digitCount;
          if !digitCount = separator_position_ && !digitCount < codeLength then
            code := !code ^ String.make 1 separator_;
        end
    done;
    !code

  let encodeGrid (latitude : float) (longitude : float) (codeLength : int) : string =
    let code = ref "" in
    let latPlaceValue = ref grid_size_degrees_ in
    let lngPlaceValue = ref grid_size_degrees_ in
    let adjustedLatitude = ref (mod_float (latitude +. latitude_max_) !latPlaceValue) in
    let adjustedLongitude = ref (mod_float (longitude +. longitude_max_) !lngPlaceValue) in
    for i = 0 to codeLength - 1 do
      let row = floor (!adjustedLatitude /. (!latPlaceValue /. grid_rows_)) in
      let col = floor (!adjustedLongitude /. (!lngPlaceValue /. grid_columns_)) in
      latPlaceValue := !latPlaceValue /. grid_rows_;
      lngPlaceValue := !lngPlaceValue /. grid_columns_;
      adjustedLatitude := !adjustedLatitude -. row *. !latPlaceValue;
      adjustedLongitude := !adjustedLongitude -. col *. !lngPlaceValue;
      code := !code ^ String.make 1 code_alphabet_.[row *. grid_columns_ +. col |> int_of_float]
    done;
    !code

  let encode ?(codeLength = pair_code_length_) (latitude : float) (longitude : float) : string =
    if codeLength < 2 then
      raise Invalid_Open_Location_Code_length;
    let latitude = clipLatitude latitude in
    let longitude = normalizeLongitude longitude in
    if latitude = 90. then
      begin
        let latitude = latitude -. computeLatitudePrecision codeLength in
        let code = ref (String.make 1 prefix_) in
        code := !code ^ encodePairs latitude longitude (min codeLength pair_code_length_);
        if codeLength > pair_code_length_ then
          code := !code ^ encodeGrid latitude longitude (codeLength - pair_code_length_);
        !code
      end
    else
      begin
        let code = ref (String.make 1 prefix_) in
        code := !code ^ encodePairs latitude longitude (min codeLength pair_code_length_);
        if codeLength > pair_code_length_ then
          code := !code ^ encodeGrid latitude longitude (codeLength - pair_code_length_);
        !code
      end

  let decodePairsSequence (code : string) (offset : int) : (float * float) =
    let i = ref 0 in
    let value = ref 0. in
    while (!i * 2 + offset < String.length code) do
      value := !value +. float_of_int (String.index code_alphabet_ code.[!i * 2 + offset]) *. pair_resolutions_.(!i);
      incr i
    done;
    (!value, !value +. pair_resolutions_.(!i - 1))

  let decodePairs (code : string) : codeArea =
    let latitude = decodePairsSequence code 0 in
    let longitude = decodePairsSequence code 1 in
    create_codeArea (fst latitude -. latitude_max_) (fst longitude -. longitude_max_) (snd latitude -. latitude_max_) (snd longitude -. longitude_max_) (String.length code)

  let decodeGrid (code : string) : codeArea =
    let latitudeLo = ref 0. in
    let longitudeLo = ref 0. in
    let latPlaceValue = ref grid_size_degrees_ in
    let lngPlaceValue = ref grid_size_degrees_ in
    let i = ref 0 in
    while !i < String.length code do
      let codeIndex = String.index code_alphabet_ code.[!i] in
      let row = floor ((float_of_int codeIndex) /. grid_columns_) in
      let col = mod_float (float_of_int codeIndex) grid_columns_ in
      latPlaceValue := !latPlaceValue /. grid_rows_;
      lngPlaceValue := !lngPlaceValue /. grid_columns_;
      latitudeLo := !latitudeLo +. row *. !latPlaceValue;
      longitudeLo := !longitudeLo +. col *. !lngPlaceValue;
      incr i
    done;
    create_codeArea !latitudeLo !longitudeLo (!latitudeLo +. !latPlaceValue) (!longitudeLo +. !lngPlaceValue) (String.length code)

  let decode (code : string) : codeArea =
    if not (isFull code) then
      failwith ("IllegalArgumentException: Passed Open Location Code is not a valid full code: " ^ code);
    if String.contains code prefix_ then
      let code = String.sub code 1 (String.length code - 1) in
      if String.contains code separator_ then
        let code = String.sub code 0 (String.index code separator_) ^ String.sub code (String.index code separator_ + 1) (String.length code - 1 - String.index code separator_) in
        let code = String.uppercase code in
        if String.length code <= pair_code_length_ then
          decodePairs code
        else
          let codeArea = decodePairs code in
          let gridArea = decodeGrid (String.sub code (pair_code_length_ + 1) (String.length code - pair_code_length_)) in
          create_codeArea (codeArea.latitudeLo +. gridArea.latitudeLo) (codeArea.longitudeLo +. gridArea.longitudeLo) (codeArea.latitudeLo +. gridArea.latitudeHi) (codeArea.longitudeLo +. gridArea.longitudeHi) (codeArea.codeLength + gridArea.codeLength)
      else
        if String.length code <= pair_code_length_ then
          decodePairs code
        else
          let codeArea = decodePairs code in
          let gridArea = decodeGrid (String.sub code (pair_code_length_ + 1) (String.length code - pair_code_length_)) in
          create_codeArea (codeArea.latitudeLo +. gridArea.latitudeLo) (codeArea.longitudeLo +. gridArea.longitudeLo) (codeArea.latitudeLo +. gridArea.latitudeHi) (codeArea.longitudeLo +. gridArea.longitudeHi) (codeArea.codeLength + gridArea.codeLength)
    else
      if String.length code <= pair_code_length_ then
        decodePairs code
      else
        let codeArea = decodePairs code in
        let gridArea = decodeGrid (String.sub code (pair_code_length_ + 1) (String.length code - pair_code_length_)) in
        create_codeArea (codeArea.latitudeLo +. gridArea.latitudeLo) (codeArea.longitudeLo +. gridArea.longitudeLo) (codeArea.latitudeLo +. gridArea.latitudeHi) (codeArea.longitudeLo +. gridArea.longitudeHi) (codeArea.codeLength + gridArea.codeLength)
end
