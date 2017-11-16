module type Backend = sig
  val show: ?figuresPath:string -> ?width:int -> ?height:int -> ?selfContained:bool -> ?figureName:string -> VegaLite.V2.TopLevelExtendedSpec.t -> (string, string) result

  val close : string -> (unit, string) result
end

let assets = [ "babel-polyfill.js"; "vega.js"; "vega-lite.js"; "vega-embed.js";  ]

type html = [
  | `Doctype of string
  | `Text of string
  | `Script of [`Url of string | `Crunch of string]
  | `Element of (string * (string * string) list * html list)
]

let node_of_tag : html -> html Markup.node = function
  | `Doctype s -> `Doctype Markup.({doctype_name=Some s; public_identifier=None; system_identifier=None; raw_text=None; force_quirks=false})
  | `Text s -> `Text s
  | `Script (`Url u) -> `Element (("", "script"), [(("", "src"), u)], [])
  | `Script (`Crunch s) -> `Element (("", "script"), [], [`Text s])
  | `Element (name, attrs, children) ->
    let attrs_ = List.map (fun (k, v) -> (("", k), v)) attrs in
    `Element (("", name), attrs_, children)

let to_html ?(selfContained=false) ?(figureName="Figure 1") spec =

  let url_script url =
    `Element ("script", [("src", url)], [])
  in
  let crunch_script fname =
    match VegaLiteAssets.read fname with | Some s -> `Element ("script", [], [`Text s])
    (* The failure case should be statically guaranteed to not happen *)
  in

  (* TODO: Rather than listing these just list files in the crunch *)
  let scripts = match selfContained with
    | true -> List.map crunch_script assets
    | false -> List.map url_script assets
  in

  let head = [
    `Element ("title", [], [`Text figureName]);
    `Element ("meta", [("charset", "utf-8")], []);
    `Element ("style", [("media", "screen")], [`Text ".vega-actions a {\n    margin-right: 5px;}"])
  ] @ scripts
  in
  let idStr = "aplomb" in
  let specStr = spec |> VegaLite.V2.TopLevelExtendedSpec.to_yojson |> Yojson.Safe.to_string in
  let specOptStr = `Assoc [("actions", `Assoc [("export", `Bool true); ("source", `Bool false); ("editor", `Bool (not selfContained))])] |> Yojson.Safe.to_string in
  let vizCode = "\nvar v1Spec = " ^ specStr ^ ";\nvegaEmbed(\"#" ^ idStr ^ "\", v1Spec, " ^ specOptStr ^ ").then(function(result){}).catch(console.error);" in
  let body = [
    `Element ("div", [("id", idStr)], []);
    `Element ("script", [], [`Text vizCode])
  ]
  in
  let nodes = [
    `Doctype "html";
    `Element ("head", [], head);
    `Element ("body", [], body)
  ]
  in
  let open Markup in
  let streams = List.map (from_tree node_of_tag) nodes in
  List.map to_list streams |> List.concat |> of_list

let to_string ?(selfContained=false) ?(figureName="Figure 1") spec =
  let open Markup in
  let stream = to_html ~selfContained ~figureName spec  in
  stream |> write_html |> to_string
