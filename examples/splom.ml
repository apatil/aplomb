(*
  To run interactively:

  utop -require aplomb.ppx_deriving,aplomb,ppx_deriving_yojson,yojson,aplomb.browser -init examples/splom.ml
*)

(* Set up the row type and derive a plotting module. *)
type row = {
  name : string [@typ `Nominal];
  mpg : float option [@typ `Quantitative];
  cylinders : int [@typ `Quantitative];
  displacement : float [@typ `Quantitative];
  horsepower : int option [@typ `Quantitative];
  weight_lbs : int [@typ `Quantitative];
  acceleration : float [@typ `Quantitative];
  year : string [@typ `Temporal];
  origin : string [@typ `Nominal];
} [@@deriving yojson,aplomb {aplombName="Plot_cars"}]

(* Read the data. This will be streamlined in the future, when OCaml gets dataframe support. *)
exception ParseError of string
let data = match Yojson.Safe.from_file "examples/cars.json" with
  | exception _ -> raise @@ ParseError "Failed to read data file"
  | `List jsons ->
    let rowReader el = match row_of_yojson el with
      | Ok r -> r
      | Error e -> raise @@ ParseError ("Failed to read row: " ^ (Yojson.Safe.pretty_to_string el))
    in
    jsons |> List.map rowReader |> Array.of_list
  | _ -> raise @@ ParseError "Data file did not contain a json list"



let spec = Plot_cars.(
    simple ~width:(`Int 320) ~height:(`Int 200) (`Mark `Point)
    |> xRep ~typ:`Quantitative `Column
    |> yRep ~typ:`Quantitative `Row
    |> repeat ~data ~repeat:{column=[`mpg; `acceleration; `horsepower]; row=[`horsepower; `mpg; `acceleration]}
    |> finish
  )


let _ = AplombBrowser.show spec
