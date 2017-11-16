val to_html : ?selfContained:bool -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (Markup.signal, Markup.sync) Markup.stream
val to_string : ?selfContained:bool -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> string

module type Backend = sig
  val show: ?figuresPath:string -> ?width:int -> ?height:int -> ?selfContained:bool -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (string, string) result

  val close : string -> (unit, string) result
end
