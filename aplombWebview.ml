let (>>=) (x : ('a, 'e) result) (f : 'a -> ('b, 'e) result) : ('b, 'e) result =
  match x with
  | Ok x_ -> f x_
  | e -> e

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

let show ?figuresPath ?width ?height ?figureName:(figName="Figure 1") spec  =
  ViewCommon.writeFigureFiles ?figuresPath ~assets:`Local ~figureName:figName spec >>= fun url ->

  (match Hashtbl.find figures figName with
   | exception _ -> newFigure ?width ?height url figName
   | wvt -> updateFigure wvt figName)  >>= fun _ ->

  Ok figName

let close figureName =
  match Hashtbl.find figures figureName with
  | exception _ -> Ok ()
  | wvt -> Ok (Webview.exit wvt)
