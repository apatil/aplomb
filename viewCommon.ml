let asset_fnames = [ "babel-polyfill.js"; "vega.js"; "vega-lite.js"; "vega-embed.js";  ]
let remote_assets = [
  "https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.26.0/polyfill.js";
  "https://cdnjs.cloudflare.com/ajax/libs/vega/3.0.7/vega.js";
  "https://cdnjs.cloudflare.com/ajax/libs/vega-lite/2.0.1/vega-lite.js";
  "https://cdnjs.cloudflare.com/ajax/libs/vega-embed/3.0.0-rc7/vega-embed.js";
]

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

let to_html ?(assets=`Remote) ?(figureName="Figure 1") spec =

  let url_script url =
    `Element ("script", [("src", url)], [])
  in
  let crunch_script fname =
    match VegaLiteAssets.read fname with | Some s -> `Element ("script", [], [`Text s])
    (* The failure case should be statically guaranteed to not happen *)
  in

  (* TODO: Rather than listing these just list files in the crunch *)
  let scripts = match assets with
    | `Inline -> List.map crunch_script asset_fnames
    | `Local -> List.map url_script asset_fnames
    | `Remote -> List.map url_script remote_assets
  in

  let head = [
    `Element ("title", [], [`Text figureName]);
    `Element ("meta", [("charset", "utf-8")], []);
    `Element ("style", [("media", "screen")], [`Text ".vega-actions a {\n    margin-right: 5px;}"])
  ] @ scripts
  in
  let idStr = "aplomb" in
  let specStr = spec |> VegaLite.V2.TopLevelExtendedSpec.to_yojson |> Yojson.Safe.to_string in
  let specOptStr = `Assoc [("actions", `Assoc [("export", `Bool true); ("source", `Bool true); ("editor", `Bool true)])] |> Yojson.Safe.to_string in
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

let to_string ?assets ?figureName spec =
  let open Markup in
  let stream = to_html ?assets ?figureName spec  in
  stream |> write_html |> to_string

let (>>=) (x : ('a, 'e) result) (f : 'a -> ('b, 'e) result) : ('b, 'e) result =
  match x with
  | Ok x_ -> f x_
  | e -> e

let ensureDir pth  : (string, string) result =
  let mkdir = fun () ->
    try let _ = Unix.mkdir pth 0o750 in Ok "ok"
    with Sys_error msg -> Error ("Failed to mkdir: " ^ msg)
  in

  match Sys.is_directory pth with
  | exception _ -> mkdir ()
  | false -> mkdir ()
  | true -> Ok "ok"

let ensureAssets pth : (string, string) result =
  let reducer sofar fname = match sofar with
    | Error e -> Error e
    | Ok _ -> (let filePath = Filename.concat pth fname in
               match Sys.file_exists filePath with
               | true -> Ok "ok"
               | false -> (match VegaLiteAssets.read fname with
                   | None -> Error "Asset not found"
                   | Some content ->
                     try
                       let oc = open_out filePath in
                       let _ = Printf.fprintf oc "%s\n" content in
                       let _ = close_out oc in
                       Ok "ok"
                     with _ -> Error "Could not write file"))
  in
  List.fold_left reducer (Ok "ok") VegaLiteAssets.file_list

let writeFigureFiles ?figuresPath ?(assets=`Local) ?(figureName="Figure 1") spec =
  let stream = to_html ~assets ~figureName spec |> Markup.write_html in
  let figuresPath_ = match figuresPath with
    | Some p -> p
    | None -> Filename.(concat (get_temp_dir_name ()) "aplomb" )
  in

  let pth = Filename.concat figuresPath_ (figureName ^ ".html") in
  let pthfwd = Str.(global_replace (regexp Filename.dir_sep) "/" pth) in

  let url = Uri.make ~scheme:"file" ~host:"" ~path:pthfwd () |> Uri.to_string in

  (match Filename.is_relative pth with
   | true -> Error "figuresPath must be absolute"
   | false -> Ok url) >>= fun _ ->

  ensureDir figuresPath_ >>= fun _ ->

  (match assets with
   | `Remote | `Inline -> Ok url
   | `Local -> ensureAssets figuresPath_) >>= fun _ ->

  (match Markup.to_file pth stream with
   | exception (Sys_error msg) -> Error ("Failed to write: " ^ msg)
   | _ -> Ok url)
