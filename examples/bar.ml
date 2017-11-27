(*
  To run interactively:

  utop -require aplomb,aplomb.browser -init examples/bar.ml
*)

let data = [
  ("a", `String [| "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I" |]);
  ("b", `Int [| 28; 55; 43; 91; 81; 53; 19; 87; 52 |])
]

let spec =
  Aplomb.Dynamic.(simple ~data ~description:"A simple bar chart with embedded data." (`Mark `Bar)
    |> x "a" ~typ:`Ordinal
    |> y "b" ~typ:`Quantitative
    |> finish)

let _ = AplombBrowser.show spec
