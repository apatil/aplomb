(*
  To run interactively:

  utop -require aplomb.ppx_deriving,aplomb,ppx_deriving_yojson,yojson,aplomb.browser -init examples/bar_static.ml
*)
type row = {
    a : string [@typ `Ordinal];
    b : int [@typ `Quantitative]
  } [@@deriving yojson,aplomb]

let data =
  [|
    {a = "A"; b = 28};
    {a = "B"; b = 55};
    {a = "C"; b = 43};
    {a = "D"; b = 91};
    {a = "E"; b = 81};
    {a = "F"; b = 53};
    {a = "G"; b = 19};
    {a = "H"; b = 87};
    {a = "I"; b = 52}
  |]

let spec =
  Plot_row.(simple ~data ~description:"A simple bar chart with embedded data." (`Mark `Bar)
    |> x `a
    |> y `b
    |> finish)

let _ = AplombBrowser.show spec
