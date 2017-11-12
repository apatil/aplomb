module type RowS = sig
  type t
  val to_yojson : t -> Yojson.Safe.json
  type field
  val fieldName : field -> string
  val typ : field -> VegaLite.V2.Type.t
end

module Make (R : RowS) = struct
  open VegaLite.V2

  type field = R.field
  type row = R.t

  let useData ?format rows =
    `Inline (InlineData.make ?format ~values:(`Jsons (List.map R.to_yojson rows)) ())

  let otherData ?format x = match x with
    | `Jsons l -> `Inline (InlineData.make ?format ~values:(`Jsons l) ())
    | `String s -> `Inline (InlineData.make ?format ~values:(`String s) ())
    | `Url url -> `Url (UrlData.make ?format ~url ())
    | `Named name -> `Named (NamedData.make ?format ~name ())


  (* Types for building blocks of encodings based on concrete fields *)

  let field x = `String (R.fieldName x)

  let inferTyp typ_ field =
    match typ_ with
    | Some tp -> tp
    | None -> (R.typ field)

  let repeatField x =
    match x with
    | `Row -> (`Repeat (`Row))
    | `Column -> (`Repeat (`Column))


  type repeat = {
    column : field list;
    row : field list;
  }

  let fieldDef ?aggregate ?bin ?timeUnit ?typ ~field:x () =
    FieldDef.make
      ?aggregate
      ?bin
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let positionFieldDef ?aggregate ?axis ?bin ?scale ?sort ?stack ?timeUnit ?typ ~field:x () =
    PositionFieldDef.make
      ?aggregate
      ?axis
      ?bin
      ?scale
      ?sort
      ?stack
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let conditionalLegendFieldDef ?aggregate ?bin ?condition ?legend ?scale ?sort ?timeUnit ?typ ~field:x () =
    ConditionalLegendFieldDef.make
      ?aggregate
      ?bin
      ?condition
      ?legend
      ?scale
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let conditionalTextFieldDef ?aggregate ?bin ?condition ?format ?timeUnit ?typ ~field:x () =
    ConditionalTextFieldDef.make
      ?aggregate
      ?bin
      ?condition
      ?format
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let orderFieldDef ?aggregate ?bin ?sort ?timeUnit ?typ ~field:x () =
    OrderFieldDef.make
      ?aggregate
      ?bin
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let conditionLegendFieldDef ?aggregate ?bin ?legend ?scale ?sort ?timeUnit ?typ ~selection ~field:x () =
    ConditionLegendFieldDef.make
      ?aggregate
      ?bin
      ?legend
      ?scale
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~selection
      ~field:(field x)
      ()

  let conditionTextFieldDef ?aggregate ?bin ?format ?timeUnit ?typ ~selection ~field:x () =
    ConditionTextFieldDef.make
      ?aggregate
      ?bin
      ?format
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~selection
      ~field:(field x)
      ()

  let facetFieldDef ?aggregate ?bin ?header ?sort ?timeUnit ?typ ~field:x () =
    FacetFieldDef.make
      ?aggregate
      ?bin
      ?header
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let legendFieldDef ?aggregate ?bin ?legend ?scale ?sort ?timeUnit ?typ ~field:x () =
    LegendFieldDef.make
      ?aggregate
      ?bin
      ?legend
      ?scale
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let scaleFieldDef ?aggregate ?bin ?scale ?sort ?timeUnit ?typ ~field:x () =
    ScaleFieldDef.make
      ?aggregate
      ?bin
      ?scale
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let orderFieldDef ?aggregate ?bin ?sort ?timeUnit ?typ ~field:x () =
    OrderFieldDef.make
      ?aggregate
      ?bin
      ?sort
      ?timeUnit
      ~typ:(inferTyp typ x)
      ~field:(field x)
      ()

  let legendCondition ?aggregate ?bin ?legend ?scale ?sort ?timeUnit ?typ ~selection x =
    `Field (conditionLegendFieldDef ?aggregate ?bin ?legend ?scale ?sort ?timeUnit ?typ ~selection ~field:x ())

  let textCondition ?aggregate ?bin ?format ?timeUnit ?typ ~selection x =
    `Field (conditionTextFieldDef ?aggregate ?bin ?format ?timeUnit ?typ ~selection ~field:x ())

  let sort ?order ~op x =
    `Field (SortField.make
              ?order
              ~op
              ~field:(R.fieldName x)
              ())

  let rec specSet (f : Encoding.t -> Encoding.t) (spec : Spec.t) : Spec.t =
    match spec with
    | `Unit s -> `Unit CompositeUnitSpec.{s with encoding=(f s.encoding)}
    | `Repeat s -> `Repeat RepeatSpec.{s with spec = specSet f s.spec}
    | `Faceted s -> `Faceted FacetedSpec.{s with spec = specSetLayer f s.spec}
    | `Layer s -> `Layer LayerSpec.{s with layer = List.map (specSetLayer f) s.layer}
    | `HConcat s -> `HConcat HConcatSpec.{s with hconcat = List.map (specSet f) s.hconcat}
    | `VConcat s -> `VConcat VConcatSpec.{s with vconcat = List.map (specSet f) s.vconcat}

  and specSetLayer (f : Encoding.t -> Encoding.t) (spec : [`Layer of LayerSpec.t | `Unit of CompositeUnitSpec.t]) =
    match spec with
    | `Layer s -> `Layer LayerSpec.{s with layer = List.map (specSetLayer f) s.layer}
    | `Unit s -> `Unit CompositeUnitSpec.{s with encoding=(f s.encoding)}



  (* Note, encodingWithFacet only occurs in TopLevelFacetedUnitSpec . Pull it out of '.facet' during finalize.*)
  type pfdSetter =
    ?aggregate:AggregateOp.t ->
    ?axis:Axis.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?stack:StackOffset.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  let pfdSet (f : PositionFieldDef.t -> Encoding.t -> Encoding.t) : pfdSetter =
    fun ?aggregate ?axis ?bin ?scale ?sort ?stack ?timeUnit ?typ x spec ->
      let pfd = positionFieldDef ?aggregate ?axis ?bin ?scale ?sort ?stack ?timeUnit ?typ ~field:x () in
      specSet (f pfd) spec

  let x =
    let setter pfd e = Encoding.{e with x = Some (`Field pfd)} in
    pfdSet setter

  let xVal v spec =
    let f enc = Encoding.{enc with x = Some (`Value v)} in
    specSet f spec

  let y =
    let setter pfd e = Encoding.{e with y = Some (`Field pfd)} in
    pfdSet setter

  let yVal v spec =
    let f enc = Encoding.{enc with y = Some (`Value v)} in
    specSet f spec


  type fdSetter =
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  let fdSet (f : FieldDef.t -> Encoding.t -> Encoding.t) : fdSetter =
    fun ?aggregate ?bin ?timeUnit ?typ x spec ->
      let fd = fieldDef ?aggregate ?bin ?timeUnit ?typ ~field:x () in
      specSet (f fd) spec

  let x2 =
    let setter fd e = Encoding.{e with x2 = Some (`Field fd)} in
    fdSet setter

  let x2Val v spec =
    let f enc = Encoding.{enc with x2 = Some (`Value v)} in
    specSet f spec

  let y2 =
    let setter fd e = Encoding.{e with x2 = Some (`Field fd)} in
    fdSet setter

  let y2Val v spec =
    let f enc = Encoding.{enc with y2 = Some (`Value v)} in
    specSet f spec


  type clfdSetter =
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?legend:Legend.t ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  let clfdSet (f : ConditionalLegendFieldDef.t -> Encoding.t -> Encoding.t) : clfdSetter =
    fun ?aggregate ?bin ?condition ?legend ?scale ?sort ?timeUnit ?typ x spec ->
      let clfd = conditionalLegendFieldDef ?aggregate ?bin ?condition ?legend ?scale ?sort ?timeUnit ?typ ~field:x () in
      specSet (f clfd) spec

  let size =
    let setter clfd e = Encoding.{e with size = Some (`Field clfd)} in
    clfdSet setter

  let sizeVal ?condition ?value spec =
    let v = ConditionalLegendValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with size = Some (`Value v)} in
    specSet f spec

  let shape =
    let setter clfd e = Encoding.{e with shape = Some (`Field clfd)} in
    clfdSet setter

  let shapeVal ?condition ?value spec =
    let v = ConditionalLegendValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with shape = Some (`Value v)} in
    specSet f spec

  let opacity =
    let setter clfd e = Encoding.{e with opacity = Some (`Field clfd)} in
    clfdSet setter

  let opacityVal ?condition ?value spec =
    let v = ConditionalLegendValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with opacity = Some (`Value v)} in
    specSet f spec

  let color =
    let setter clfd e = Encoding.{e with color = Some (`Field clfd)} in
    clfdSet setter

  let colorVal ?condition ?value spec =
    let v = ConditionalLegendValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with color = Some (`Value v)} in
    specSet f spec


  type ctfdSetter =
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  let ctfdSet (f : ConditionalTextFieldDef.t -> Encoding.t -> Encoding.t) : ctfdSetter =
    fun ?aggregate ?bin ?condition ?format ?timeUnit ?typ x spec ->
      let ctfd = conditionalTextFieldDef ?aggregate ?bin ?condition ?format ?timeUnit ?typ ~field:x () in
      specSet (f ctfd) spec

  let tooltip =
    let setter ctfd e = Encoding.{e with tooltip = Some (`Field ctfd)} in
    ctfdSet setter

  let tooltipVal ?condition ?value spec =
    let v = ConditionalTextValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with tooltip = Some (`Value v)} in
    specSet f spec

  let text =
    let setter ctfd e = Encoding.{e with text = Some (`Field ctfd)} in
    ctfdSet setter

  let textVal ?condition ?value spec =
    let v = ConditionalTextValueDef.make ?condition ?value () in
    let f enc = Encoding.{enc with text = Some (`Value v)} in
    specSet f spec

  let order ?aggregate ?bin ?sort ?timeUnit ?typ x spec =
    let o = orderFieldDef ?aggregate ?bin ?sort ?timeUnit ?typ ~field:x () in
    let f enc = Encoding.{enc with order = Some (`Field o)} in
    specSet f spec

  let orderMulti ofds spec =
    let f enc = Encoding.{enc with order = Some (`Fields ofds)} in
    specSet f spec

  let detail ?aggregate ?bin ?timeUnit ?typ x spec =
    let o = fieldDef ?aggregate ?bin ?timeUnit ?typ ~field:x () in
    let f enc = Encoding.{enc with detail = Some (`Field o)} in
    specSet f spec

  let detailMulti fds spec =
    let f enc = Encoding.{enc with detail = Some (`Fields fds)} in
    specSet f spec

  let data_ : row list option -> Data.t option = function
    | None -> None
    | Some d -> Some (useData d)

  let repeat_ : repeat -> Repeat.t = function
    | Repeat.{row; column} -> Repeat.{
        row = Some (List.map R.fieldName row);
        column = Some (List.map R.fieldName column);
      }

  let unitSpec ?description ?height ?name ?selection ?title ?transform ?width ?encoding ?data ~mark () =
    let e = match encoding with
      | None -> (Encoding.make ())
      | Some e -> e
    in
    CompositeUnitSpec.make ?description ?height ?name ?selection ?title ?transform ?width ?data:(data_ data) ~encoding:e ~mark ()

  let layerSpec ?description ?height ?name ?resolve ?title ?transform ?width ?data ~layer () =
    LayerSpec.make ?description ?height ?name ?resolve ?title ?transform ?width ?data:(data_ data) ~layer ()

  let faceted ?description ?name ?resolve ?title ?transform ?data ~facet ~spec () =
    `Faceted (FacetedSpec.make ?description ?name ?resolve ?title ?transform ?data:(data_ data) ~facet ~spec ())

  let hconcat ?description ?name ?resolve ?title ?transform ?data ~hconcat () =
    `HConcat (HConcatSpec.make ?description ?name ?resolve ?title ?transform ?data:(data_ data) ~hconcat ())

  let vconcat ?description ?name ?resolve ?title ?transform ?data ~vconcat () =
    `VConcat (VConcatSpec.make ?description ?name ?resolve ?title ?transform ?data:(data_ data) ~vconcat ())

  let repeat ?description ?name ?resolve ?title ?transform ?data ~repeat ~spec () =
    `Repeat (RepeatSpec.make ?description ?name ?resolve ?title ?transform ?data:(data_ data) ~repeat:(repeat_ repeat) ~spec ())

  let simple ?description ?height ?name ?selection ?title ?transform ?width ?encoding ?data ~mark () =
    `Unit (unitSpec ?description ?height ?name ?selection ?title ?transform ?width ?encoding ?data ~mark ())

  let layer ?description ?height ?name ?resolve ?title ?transform ?width ?data ~layer () =
    `Layer (layerSpec ?description ?height ?name ?resolve ?title ?transform ?width ?data ~layer ())

  let facetize row column = function
    | Encoding.{x; y; x2; y2; tooltip; text; size; shape; order; opacity; detail; color} ->
      EncodingWithFacet.make ?x ?y ?x2 ?y2 ?tooltip ?text ?size ?shape ?order ?opacity ?detail ?color ?row ?column ()

  let toplevel ?padding ?config ?background ?autoResize ?rowFacet ?columnFacet : (Spec.t -> TopLevelExtendedSpec.t) = function

    | `Unit CompositeUnitSpec.{width; transform; title; selection; name; mark; height; encoding; description; data} ->
      let encoding = facetize rowFacet columnFacet encoding in
      `Unit (TopLevelFacetedUnitSpec.make ?width ?transform ?title ?selection ?name ~mark ?height ~encoding ?description ?data ?padding ?config ?background ?autoResize ())

    | `Faceted FacetedSpec.{transform; title; spec; resolve; name; facet; description; data} ->
      `Faceted (TopLevelFacetedSpec.make ?transform ?title ~spec ?resolve ?name ~facet ?description ?data ?padding ?config ?background ?autoResize ())

    | `Layer LayerSpec.{width; transform; title; resolve; name; layer; height; description; data} ->
      `Layer (TopLevelLayerSpec.make ?width ?transform ?title ?resolve ?name ~layer ?height ?description ?data ?padding ?config ?background ?autoResize ())

    | `Repeat RepeatSpec.{transform; title; spec; resolve; repeat; name; description; data} ->
      `Repeat (TopLevelRepeatSpec.make ?transform ?title ~spec ?resolve ~repeat ?name ?description ?data ?padding ?config ?background ?autoResize ())

    | `HConcat HConcatSpec.{transform; title; resolve; name; hconcat; description; data} ->
      `HConcat (TopLevelHConcatSpec.make ?transform ?title ?resolve ?name ~hconcat ?description ?data ?padding ?config ?background ?autoResize ())

    | `VConcat VConcatSpec.{transform; title; resolve; name; vconcat; description; data} ->
      `VConcat (TopLevelVConcatSpec.make ?transform ?title ?resolve ?name ~vconcat ?description ?data ?padding ?config ?background ?autoResize ())


  let to_yojson = TopLevelExtendedSpec.to_yojson

end
