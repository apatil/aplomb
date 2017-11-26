open Parsetree
open Asttypes

let ident_of_name (name : string) : Longident.t Asttypes.loc =
  Asttypes.{txt = Longident.Lident name; loc = Location.none}

let expr_of_desc (desc : Parsetree.expression_desc) : Parsetree.expression =
  Parsetree.{
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_attributes = []
  };;

let expr_of_string (s : string) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc @@ Pexp_constant (Pconst_string (s, None))

let ident_expr (name : string) : Parsetree.expression =
  expr_of_desc @@ Parsetree.Pexp_ident (ident_of_name name)

let type_of_desc (desc: Parsetree.core_type_desc) : Parsetree.core_type =
  Parsetree.{
    ptyp_desc = desc;
    ptyp_loc = Location.none;
    ptyp_attributes = [];
  }

let type_of_name (typ : string) : Parsetree.core_type =
  type_of_desc @@ Parsetree.Ptyp_constr (ident_of_name typ, [])

let pattern_of_desc (desc : Parsetree.pattern_desc) : Parsetree.pattern =
  Parsetree.{
    ppat_desc = desc;
    ppat_loc = Location.none;
    ppat_attributes = [];
  };;

let module_expr_of_desc (desc : Parsetree.module_expr_desc) =
  Parsetree.{
    pmod_desc = desc;
    pmod_loc = Location.none;
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

let fieldsType labels =
  type_of_desc @@ Parsetree.Ptyp_variant (labelVariants labels, Asttypes.Closed, None)

let typFunc labels =
  let mapper Parsetree.{pld_name = Asttypes.{txt=name}; pld_attributes = attrs; pld_type=typ} =
    let rhs = match typ with
      | [%type: int] | [%type: int32] | [%type: int64] -> [%expr `Quantitative]
      | [%type: float] -> [%expr `Quantitative]
      | [%type: string] | [%type: char] -> [%expr `Nominal]
      | [%type: bool] -> [%expr `Ordinal]
      | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Cannot infer Vega-Lite type for OCaml type '%s'. Please add an annotation to field '%s', eg [@typ `Ordinal]." (Ppx_deriving.string_of_core_type typ) name
    in
    let rhs = match List.find_all (fun (Asttypes.{txt=attrName}, _) -> attrName = "typ") attrs with
      | [] -> rhs
      | [(_, PStr [%str `Ordinal])] -> [%expr `Ordinal]
      | [(_, PStr [%str `Temporal])] -> [%expr `Temporal]
      | [(_, PStr [%str `Quantitative])] -> [%expr `Quantitative]
      | [(_, PStr [%str `Nominal])] -> [%expr `Nominal]
      | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Please provide one @typ annotation with payload `Ordinal, `Temporal, `Quantitative or `Nominal. For example, [@typ `Nominal]."
    in
    Parsetree.{
      pc_lhs = pattern_of_desc @@ (Ppat_variant (name, None));
      pc_guard = None;
      pc_rhs = rhs;
    }
  in
  expr_of_desc @@ Parsetree.Pexp_function (List.map mapper labels)

let fieldNameFunc labels =
  let mapper Parsetree.{pld_name = Asttypes.{txt=name}} =
    Parsetree.{
      pc_lhs = pattern_of_desc @@ (Ppat_variant (name, None));
      pc_guard = None;
      pc_rhs = [%expr [%e (expr_of_string name)]];
    }
  in
  expr_of_desc @@ Parsetree.Pexp_function (List.map mapper labels)

let getModName name options =
  match List.find (fun (n, v) -> n = "name") options with
    | exception _ -> "Plot_" ^ name
    | (n, v) -> match v with
      | Parsetree.{pexp_desc = Pexp_constant (Pconst_string (s, _))} -> s
      | _ -> "Plot_" ^ name

let getNameAndLabels tdecl =
  match tdecl with
    | Parsetree.{ptype_name = Asttypes.{txt = name}; ptype_kind = Ptype_record labels} -> (name, labels)
    | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Can only transform a record type"

let type_decl_str ~options ~path tdecls =
  let tdecl = check ~options ~path tdecls in
  let (name, labels) = getNameAndLabels tdecl in
  let modName = getModName name options in
  let fieldType = fieldsType labels in
  let row_to_yojson = match name with
    | "t" -> ident_expr "to_yojson"
    | _ -> ident_expr (name ^ "_to_yojson")
  in
  let dataModule = [%str
    type field = [%t fieldType]
    type t = [%t type_of_name name] array
    let inline x =
      let open Array in
      `List (x |> map [%e row_to_yojson] |> to_list)
    let fieldName = [%e fieldNameFunc labels]
    let defaultType = [%e typFunc labels]
  ]
  in
  let aplombExpr = module_expr_of_desc (Pmod_ident (ident_of_name "Aplomb")) in
  let aplombArg = module_expr_of_desc (Pmod_structure dataModule) in
  Parsetree.[
    {
      pstr_loc = Location.none;
      pstr_desc = Pstr_module {
          pmb_name = Asttypes.{txt = modName; loc = Location.none};
          pmb_expr = module_expr_of_desc (Pmod_apply (aplombExpr, aplombArg));
          pmb_attributes = [];
          pmb_loc = Location.none;
        }
    }
  ]


let type_decl_sig ~options ~path tdecls =
  let tdecl = check ~options ~path tdecls in
  let (name, labels) = getNameAndLabels tdecl in
  let modName = getModName name options in
  let fieldType = fieldsType labels in
  [%sig:
    module Plot:AplombS
  ]

let () = Ppx_deriving.(
  let deriver = create "aplomb"
      ~type_decl_str
      ~type_decl_sig
      ()
  in
  register deriver
)
