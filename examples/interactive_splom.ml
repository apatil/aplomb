(*
  To run interactively:

  utop -require aplomb.ppx_deriving,aplomb,ppx_deriving_yojson,yojson,aplomb.browser -init examples/interactive_splom.ml
*)


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
} [@@deriving yojson,aplomb {aplombName = "Plot_cars"}]

(* Data reading will be streamlined in future, once OCaml gets dataframe support. *)
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



(* Configure interaction with the plot. See https://vega.github.io/vega-lite/docs/selection.html *)
let selection =
  let brushSel = VegaLite.V2.IntervalSelection.make
    ~typ:`Interval
    ~resolve:`Union
    ~on:(`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
    ~translate:(`String "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!")
    ~zoom:(`String "wheel![event.shiftKey]")
    ()
  in

  let gridSel = VegaLite.V2.IntervalSelection.make
    ~typ:`Interval
    ~resolve:`Global
    ~bind:`Scales
    ~translate:(`String "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!")
    ~zoom:(`String "wheel![!event.shiftKey]")
    ()
  in

  [
    ("brush", (`Interval brushSel));
    ("grid", (`Interval gridSel))
  ]


let spec =

  let colorCond =
    Plot_cars.conditionLegendFieldDef ~typ:`Nominal ~selection:(`String "brush") `origin
  in

  Plot_cars.(
    simple ~width:(`Int 320) ~height:(`Int 200) ~selection (`Mark `Point)
    |> xRep ~typ:`Quantitative `Column
    |> yRep ~typ:`Quantitative `Row
    |> colorVal ~value:(`String "grey") ~condition:colorCond
    |> repeat ~data ~repeat:{column=[`mpg; `acceleration; `horsepower]; row=[`horsepower; `mpg; `acceleration]}
    |> finish
  )


let _ = AplombBrowser.show spec
