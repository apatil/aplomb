let (>>=) (x : ('a, 'e) result) (f : 'a -> ('b, 'e) result) : ('b, 'e) result =
  match x with
  | Ok x_ -> f x_
  | e -> e

let show ?figuresPath ?figureName spec  =
  ViewCommon.writeFigureFiles ?figuresPath ~assets:`Local ?figureName spec >>= fun url ->
  match Webbrowser.reload url with
  | Error (`Msg e) -> Error e
  | Ok _ -> Ok url
