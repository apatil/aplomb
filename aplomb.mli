module type RowS = sig
  type t
  val to_yojson : t -> Yojson.Safe.json
  type field
  val fieldName : field -> string
  val typ : field -> VegaLite.V2.Type.t
end

module Make : functor (R : RowS) -> sig
  open VegaLite.V2
  type field = R.field
  type row = R.t

  val otherData : ?format:DataFormat.t -> [ `Jsons of (Yojson.Safe.json list) | `String of string | `Url of string | `Named of string ] -> Data.t

  val field : field -> Field.t

  val useData : ?format:DataFormat.t -> row list -> Data.t

  val fieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> FieldDef.t

  type repeat = {
    column : field list;
    row : field list;
  }

  val positionFieldDef :
    ?aggregate:AggregateOp.t ->
    ?axis:Axis.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?stack:StackOffset.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> PositionFieldDef.t

  val conditionalLegendFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?legend:Legend.t ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> ConditionalLegendFieldDef.t

  val conditionalTextFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> ConditionalTextFieldDef.t

  val orderFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?sort:SortOrder.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> OrderFieldDef.t

  val conditionLegendFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?legend:Legend.t ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    selection:SelectionOperand.t ->
    field:field ->
    unit -> ConditionLegendFieldDef.t

  val conditionTextFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    selection:SelectionOperand.t ->
    field:field ->
    unit -> ConditionTextFieldDef.t

  val facetFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?header:Header.t ->
    ?sort:SortOrder.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> FacetFieldDef.t

  val legendFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?legend:Legend.t ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> LegendFieldDef.t

  val scaleFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> ScaleFieldDef.t

  val orderFieldDef :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?sort:SortOrder.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field:field ->
    unit -> OrderFieldDef.t

  (* Types for encoding fields *)
  val legendCondition :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?legend:Legend.t ->
    ?scale:Scale.t ->
    ?sort:[ `Field of SortField.t | `Order of SortOrder.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    selection:SelectionOperand.t ->
    field ->
    [ `Field of ConditionLegendFieldDef.t | `Value of ConditionValueDef.t ]

  val textCondition :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    selection:SelectionOperand.t ->
    field ->
    [ `Field of ConditionTextFieldDef.t | `Value of ConditionValueDef.t ]

  val sort :
    ?order:SortOrder.t ->
    op:AggregateOp.t ->
    field ->
    [ `Field of SortField.t | `Order of SortOrder.t ]

  val x :
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

  val xVal :
    [ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val y :
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

  val yVal :
    [ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t


  val x2 :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val x2Val :
    [ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val y2 :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val y2Val :
    [ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val size :
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

  val sizeVal :
    ?condition:[ `Field of ConditionLegendFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val shape :
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

  val shapeVal :
    ?condition:[ `Field of ConditionLegendFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val opacity :
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

  val opacityVal :
    ?condition:[ `Field of ConditionLegendFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val color :
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

  val colorVal :
    ?condition:[ `Field of ConditionLegendFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val tooltip :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val tooltipVal :
    ?condition:[ `Field of ConditionTextFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val text :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?condition:ConditionValueDef.t ->
    ?format:string ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val textVal :
    ?condition:[ `Field of ConditionTextFieldDef.t | `Value of ConditionValueDef.t ] ->
    ?value:[ `Int of int | `Float of float | `Bool of bool | `String of string ] ->
    Spec.t -> Spec.t

  val order :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?sort:SortOrder.t ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val orderMulti :
    OrderFieldDef.t list ->
    Spec.t -> Spec.t

  val detail :
    ?aggregate:AggregateOp.t ->
    ?bin:[ `Bool of bool | `Params of BinParams.t ] ->
    ?timeUnit:TimeUnit.t ->
    ?typ:Type.t ->
    field ->
    Spec.t -> Spec.t

  val detailMulti :
    FieldDef.t list ->
    Spec.t -> Spec.t



  (* For generating specs *)
  val unitSpec :
    ?description:string ->
    ?height:[ `Float of float | `Int of int ] ->
    ?name:string ->
    ?selection:(string * VegaLite.V2.SelectionDef.t) list ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?width:[ `Float of float | `Int of int ] ->
    ?encoding:Encoding.t ->
    ?data:(row list) ->
    mark:AnyMark.t ->
    unit -> CompositeUnitSpec.t

  val layerSpec :
    ?description:string ->
    ?height:[ `Float of float | `Int of int ] ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?width:[ `Float of float | `Int of int ] ->
    ?data:(row list) ->
    layer:[ `Layer of LayerSpec.t | `Unit of CompositeUnitSpec.t ] list ->
    unit -> LayerSpec.t

  val faceted :
    ?description:string ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?data:(row list) ->
    facet:Facet.t ->
    spec:[ `Layer of LayerSpec.t | `Unit of CompositeUnitSpec.t ] ->
    unit -> Spec.t

  val hconcat :
    ?description:string ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?data:(row list) ->
    hconcat:Spec.t list ->
    unit -> Spec.t

  val vconcat :
    ?description:string ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?data:(row list) ->
    vconcat:Spec.t list ->
    unit -> Spec.t

  val repeat :
    ?description:string ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?data:(row list) ->
    repeat:repeat ->
    spec:Spec.t ->
    unit -> Spec.t

  val simple :
    ?description:string ->
    ?height:[ `Float of float | `Int of int ] ->
    ?name:string ->
    ?selection:(string * VegaLite.V2.SelectionDef.t) list ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?width:[ `Float of float | `Int of int ] ->
    ?encoding:Encoding.t ->
    ?data:(row list) ->
    mark:AnyMark.t ->
    unit -> Spec.t

  val layer :
    ?description:string ->
    ?height:[ `Float of float | `Int of int ] ->
    ?name:string ->
    ?resolve:Resolve.t ->
    ?title:[ `Params of TitleParams.t | `String of string ] ->
    ?transform:Transform.t list ->
    ?width:[ `Float of float | `Int of int ] ->
    ?data:(row list) ->
    layer:[ `Layer of LayerSpec.t | `Unit of CompositeUnitSpec.t ] list ->
    unit -> Spec.t

  val toplevel :
    ?padding:Padding.t ->
    ?config:Config.t ->
    ?background:string ->
    ?autoResize:bool ->
    ?rowFacet:FacetFieldDef.t ->
    ?columnFacet:FacetFieldDef.t ->
    Spec.t -> TopLevelExtendedSpec.t

  val to_yojson: TopLevelExtendedSpec.t -> Yojson.Safe.json

end
