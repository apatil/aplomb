(** Show a Vega-Lite spec in IOCaml. *)
val show: ?context:Iocaml.cell_context -> ?figuresPath:string -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> unit
