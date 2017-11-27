(** Show a Vega-Lite spec in a webview. *)
val show: ?figuresPath:string -> ?width:int -> ?height:int -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (string, string) result

(** Close the webview connesponding to the given figure name. *)
val close : string -> (unit, string) result
