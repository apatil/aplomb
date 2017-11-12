# Plot your data with aplomb!

Aplomb is a plotting package for OCaml based on [Vega-Lite](https://github.com/apatil/ocaml-vega-lite).

Status: **pre-release**, expect breaking changes.

## Usage example

To use Aplomb, you begin by creating a plotting module that's specialized to the
type of data you're going to plot.

First, create a record type for your dataset's rows. Add ppx_deriving annotations
to generate a function that converts rows to [JSON](https://github.com/whitequark/ppx_deriving_yojson),
and a few other helper functions required by Aplomb.

You can optionally annotate the fields with Vega-Lite [types](https://vega.github.io/vega-lite/docs/type.html).

```ocaml
(*
To run this example interactively, initialize utop with

  utop -require ppx_deriving_yojson,yojson,aplomb.ppx_deriving,aplomb

Be sure you've opam installed utop, yojson, ppx_deriving_yojson, vega-lite and
aplomb.
*)

module Row = struct
  type t = {
    a : string [@typ `Ordinal];
    b : int [@typ `Quantitative]
  } [@@deriving yojson,aplomb]
end
```

Then, create the specialized plotting module.

```ocaml
module A = Aplomb.Make(Row)
```

Now, enter or import the dataset you want to plot. It should be a list of rows
of the type you created earlier.

```ocaml
let data =
  Row.([
      {a = "A"; b = 28}; {a = "B"; b = 55}; {a = "C"; b = 43};
      {a = "D"; b = 91}; {a = "E"; b = 81}; {a = "F"; b = 53};
      {a = "G"; b = 19}; {a = "H"; b = 87}; {a = "I"; b = 52}
    ])
```

Finally, use the plotting module to create a Vega-Lite [bar chart](https://vega.github.io/editor/#/examples/vega-lite/bar).

```ocaml
let spec =
  A.(simple ~data ~mark:(`Mark `Bar) ~description:"A simple bar chart with embedded data." ()
     |> x `a
     |> y `b
     |> toplevel)

let () = spec |> A.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline
```

Coming soon: visualizing the spec interactively from utop and IOCaml.
