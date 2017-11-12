(* utop -require ppx_deriving_yojson,yojson,aplomb.ppx_deriving,aplomb -init examples/bar.ml *)
module Row = struct
  type t = {
    a : string [@typ `Ordinal];
    b : int [@typ `Quantitative]
  } [@@deriving yojson,aplomb]
end

let data =
  Row.([
      {a = "A"; b = 28}; {a = "B"; b = 55}; {a = "C"; b = 43};
      {a = "D"; b = 91}; {a = "E"; b = 81}; {a = "F"; b = 53};
      {a = "G"; b = 19}; {a = "H"; b = 87}; {a = "I"; b = 52}
    ])

module A = Aplomb.Make(Row)
let spec =
  A.(simple ~data ~mark:(`Mark `Bar) ~description:"A simple bar chart with embedded data." ()
     |> x `a
     |> y `b
     |> toplevel
     |> to_yojson)
