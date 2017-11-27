(** Convert a Vega-Lite spec to HTML. Optionally inline all JavaScript assets into the HTML. *)
val to_html : ?assets:[`Local | `Remote | `Inline] -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (Markup.signal, Markup.sync) Markup.stream

(** Convert a Vega-Lite spec to an HTML string. *)
val to_string : ?assets:[`Local | `Remote | `Inline] -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> string

(** Write a Vega-Lite spec and any JavaScript assets it requires into a folder. *)
val writeFigureFiles : ?figuresPath:string -> ?assets:[`Local | `Remote | `Inline] -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (string, string) result
