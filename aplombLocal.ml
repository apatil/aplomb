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

let writeFigureFiles ?figuresPath ?width ?height ~selfContained ~figureName spec =
  let stream = ViewCommon.to_html ~selfContained ~figureName spec |> Markup.write_html in
  let figuresPath_ = match figuresPath with
    | Some p -> p
    | None -> Filename.(concat (get_temp_dir_name ()) "aplomb" )
  in

  let pth = Filename.concat figuresPath_ (figureName ^ ".html") in
  let pthfwd = Str.(global_replace (regexp Filename.dir_sep) "/" pth) in

  let url = Uri.make ~scheme:"file" ~host:"" ~path:pthfwd () |> Uri.to_string in

  let _ = print_endline ("URL: " ^ url) in

  (match Filename.is_relative pth with
   | true -> Error "figuresPath must be absolute"
   | false -> Ok url) >>= fun _ ->
  ensureDir figuresPath_ >>= fun _ ->

  (match selfContained with
   | true -> Ok url
   | false -> ensureAssets figuresPath_) >>= fun _ ->

  (match Markup.to_file pth stream with
   | exception (Sys_error msg) -> Error ("Failed to write: " ^ msg)
   | _ -> Ok url)

module Webview = struct
  let figures : (string, (Webview.t * Thread.t)) Hashtbl.t = Hashtbl.create 0
  (* Hashtbl.replace table ~key:"three" ~data:3 *)
  (* Hashtbl.find_opt table "three" *)

  let newFigure ?width ?height url figName : (string, string) result =
    let wvt = Webview.run ~title:figName ?width ?height ~resizable:true url in
    let _ = Hashtbl.replace figures figName wvt in
    Ok "ok"

  let updateFigure (wv, thr) figName : (string, string) result =
    match Webview.eval "location.reload()" wv with
    | Ok _ -> Ok "ok"
    | Error e -> Error e

  let show ?figuresPath ?width ?height ?selfContained:(sc=false) ?figureName:(figName="Figure 1") spec  =
    writeFigureFiles ?figuresPath ?width ?height ~selfContained:sc ~figureName:figName spec >>= fun url ->

    (match Hashtbl.find figures figName with
     | exception _ -> newFigure ?width ?height url figName
     | wvt -> updateFigure wvt figName)  >>= fun _ ->

    Ok figName

  let close figureName =
    match Hashtbl.find figures figureName with
    | exception _ -> Ok ()
    | wvt -> Ok (Webview.exit wvt)
end

module Webbrowser = struct
  let show ?figuresPath ?width ?height ?selfContained:(sc=false) ?figureName:(figName="Figure 1") spec  =
    writeFigureFiles ?figuresPath ?width ?height ~selfContained:sc ~figureName:figName spec >>= fun url ->
    match Webbrowser.reload url with
    | Error (`Msg e) -> Error e
    | Ok _ -> Ok figName
  let close figureName = Error "Close is not implemented for AplombLocal.WebBrowser"
end
