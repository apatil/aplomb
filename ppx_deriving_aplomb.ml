open Parsetree
open Asttypes

let expr_of_desc (desc : Parsetree.expression_desc) : Parsetree.expression =
  Parsetree.{
    pexp_desc = desc;
    pexp_loc = Location.none;
    pexp_attributes = []
  };;

let expr_of_string (s : string) : Parsetree.expression =
  let open Parsetree in
  expr_of_desc @@ Pexp_constant (Pconst_string (s, None))

let type_of_desc (desc: Parsetree.core_type_desc) : Parsetree.core_type =
  Parsetree.{
    ptyp_desc = desc;
    ptyp_loc = Location.none;
    ptyp_attributes = [];
  }

let pattern_of_desc (desc : Parsetree.pattern_desc) : Parsetree.pattern =
  Parsetree.{
    ppat_desc = desc;
    ppat_loc = Location.none;
    ppat_attributes = [];
  };;


let check ~options ~path tdecls =
  let _ = match (List.rev path) with
  | mname :: _ ->  ()
  | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Type declaration must be contained inside module"
  in
  match tdecls with
  | [tdecl] -> (match tdecl with
      | Parsetree.{ptype_name = Asttypes.{txt="t"}} -> tdecl
      | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Can only transform a type declaration with name 't'")
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


(*
  Options are like [@@deriving yojson {thing: strict}], we don't use
  Path is the enclosing modules.
*)
let type_decl_str ~options ~path tdecls =
  let tdecl = check ~options ~path tdecls in
  let labels = match tdecl with
    | Parsetree.{ptype_kind = Ptype_record labels} -> labels
    | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Can only transform a record type"
  in
  let fieldType = fieldsType labels in
  [%str
    type field = [%t fieldType]
    let fieldName = [%e fieldNameFunc labels]
    let typ = [%e typFunc labels]
  ]

let type_decl_sig ~options ~path tdecls =
  let tdecl = check ~options ~path tdecls in
  let labels = match tdecl with
    | Parsetree.{ptype_kind = Ptype_record labels} -> labels
    | _ -> Ppx_deriving.raise_errorf ~loc:Location.none "Can only transform a record type"
  in
  let fieldType = fieldsType labels in
  [%sig:
    type field = [%t fieldType]
    val fieldName : field -> string
    val typ : field -> VegaLite.V2.Type.t
  ]

let () = Ppx_deriving.(
  let deriver = create "aplomb"
      ~type_decl_str
      ~type_decl_sig
      ()
  in
  register deriver
)
