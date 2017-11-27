(** Show a Vega-Lite spec in a browser tab. *)
val show: ?figuresPath:string -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (string, string) result

(* close is not implemented for webbrowser *)
