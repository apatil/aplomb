open Parsetree
open Asttypes

let ident_of_name (loc : Location.t) (name : string) : Longident.t Asttypes.loc =
  Asttypes.{txt = Longident.Lident name; loc}

let expr_of_desc (pexp_loc : Location.t) (desc : Parsetree.expression_desc) : Parsetree.expression =
  Parsetree.{
    pexp_desc = desc;
    pexp_loc;
    pexp_attributes = []
  };;

let expr_of_string (pexp_loc : Location.t) (s : string) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc pexp_loc @@ Pexp_constant (Pconst_string (s, None))

let ident_expr (pexp_loc : Location.t) (name : string) : Parsetree.expression =
  expr_of_desc pexp_loc @@ Parsetree.Pexp_ident (ident_of_name pexp_loc name)

let type_of_desc (ptyp_loc : Location.t) (desc: Parsetree.core_type_desc) : Parsetree.core_type =
  Parsetree.{
    ptyp_desc = desc;
    ptyp_loc;
    ptyp_attributes = [];
  }

let type_of_name (ptyp_loc : Location.t) (typ : string) : Parsetree.core_type =
  type_of_desc ptyp_loc @@ Parsetree.Ptyp_constr (ident_of_name ptyp_loc typ, [])

let pattern_of_desc (ppat_loc : Location.t) (desc : Parsetree.pattern_desc) : Parsetree.pattern =
  Parsetree.{
    ppat_desc = desc;
    ppat_loc;
    ppat_attributes = [];
  };;

let module_expr_of_desc (pmod_loc : Location.t) (desc : Parsetree.module_expr_desc) =
  Parsetree.{
    pmod_desc = desc;
    pmod_loc;
    pmod_attributes = [];
  }

let check ~options ~path tdecls =
  match tdecls with
  | [tdecl] -> tdecl
(* TODO: Module type to pass to Aplomb is sig
   type t
   type field
   val inline : t -> Yojson.Safe.json list
   val fieldName : field -> string
   val defaultTyp : field -> VegaLite.V2.Type.t
  end
*)
(* TODO: Have a moduleName parameter in the deriver *)
  | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Can only transform a single type declaration"

let labelVariants labels =
  let mapper Parsetree.{pld_name = Asttypes.{txt=name}} =
    Parsetree.Rtag (name, [], false, [])
  in
  List.map mapper labels

let fieldsType loc labels =
  type_of_desc loc @@ Parsetree.Ptyp_variant (labelVariants labels, Asttypes.Closed, None)

let typFunc loc labels =
  let mapper Parsetree.{pld_name = Asttypes.{txt=name}; pld_attributes = attrs; pld_type=typ} =
    let rhs = match List.find_all (fun (Asttypes.{txt=attrName}, _) -> attrName = "typ") attrs with
      | [(_, PStr [%str `Ordinal])] -> [%expr `Ordinal]
      | [(_, PStr [%str `Temporal])] -> [%expr `Temporal]
      | [(_, PStr [%str `Quantitative])] -> [%expr `Quantitative]
      | [(_, PStr [%str `Nominal])] -> [%expr `Nominal]
      | _ -> (match typ with
        | [%type: int] | [%type: int32] | [%type: int64] -> [%expr `Quantitative]
        | [%type: float] -> [%expr `Quantitative]
        | [%type: string] | [%type: char] -> [%expr `Nominal]
        | [%type: bool] -> [%expr `Ordinal]
        | _ -> Ppx_deriving.raise_errorf ~loc "Cannot infer Vega-Lite type for OCaml type '%s'. Please add an annotation to field '%s', eg [@typ `Ordinal]." (Ppx_deriving.string_of_core_type typ) name)
    in
    Parsetree.{
      pc_lhs = pattern_of_desc loc @@ (Ppat_variant (name, None));
      pc_guard = None;
      pc_rhs = rhs;
    }
  in
  expr_of_desc loc @@ Parsetree.Pexp_function (List.map mapper labels)

let fieldNameFunc loc labels =
  let mapper Parsetree.{pld_name = Asttypes.{txt=name}} =
    Parsetree.{
      pc_lhs = pattern_of_desc loc @@ (Ppat_variant (name, None));
      pc_guard = None;
      pc_rhs = [%expr [%e (expr_of_string loc name)]];
    }
  in
  expr_of_desc loc @@ Parsetree.Pexp_function (List.map mapper labels)

let getModName name options =
  match List.find (fun (n, v) -> n = "aplombName") options with
    | exception _ -> "Plot_" ^ name
    | (n, v) -> match v with
      | Parsetree.{pexp_desc = Pexp_constant (Pconst_string (s, _))} -> s
      | _ -> "Plot_" ^ name

let getNameAndLabels tdecl =
  match tdecl with
    | Parsetree.{ptype_name = Asttypes.{txt = name}; ptype_kind = Ptype_record labels} -> (name, labels)
    | _ -> Ppx_deriving.raise_errorf ~loc:tdecl.ptype_loc "Can only transform a record type"

let type_decl_str ~options ~path tdecls =
  let tdecl = check ~options ~path tdecls in
  let loc = tdecl.ptype_loc in
  let (name, labels) = getNameAndLabels tdecl in
  let fieldType = fieldsType loc labels in
  let row_to_yojson = match name with
    | "t" -> ident_expr loc "to_yojson"
    | _ -> ident_expr loc (name ^ "_to_yojson")
  in
  let dataModule = [%str
    type field = [%t fieldType]
    type t = [%t type_of_name loc name] array
    let inline x =
      let open Array in
      let jsons = (x |> map [%e row_to_yojson] |> to_list) in
      `Inline (VegaLite.V2.InlineData.make ~values:(`Jsons jsons) ())
    let fieldName = [%e fieldNameFunc loc labels]
    let defaultTyp = [%e typFunc loc labels]
  ]
  in
  let modName = getModName name options in
  let decl = [%stri module Plot = Aplomb.Make(struct [%%s dataModule] end) ] in
  let decl_ = match decl with
  | Parsetree.{pstr_desc = Pstr_module binding} ->
    Parsetree.{decl with pstr_desc = Pstr_module {binding with pmb_name = Asttypes.{txt = modName; loc=loc}}}
  (* Other cases can't happen *)
  in
  [%str [%%s [decl_]]]



let type_decl_sig ~options ~path tdecls =
  [%sig:
    module Plot:Aplomb.AplombS
  ]

let () = Ppx_deriving.(
  let deriver = create "aplomb"
      ~type_decl_str
      ~type_decl_sig
      ()
  in
  register deriver
)
