module G = Gospel
open G.Uast

exception Not_Supported

let fname = ref None

let usage_msg = "toy_gospel <file>.ml"

let usage () =
  Arg.usage [] usage_msg;
  exit 1

let set_file f =
  match !fname with
  | None when Filename.check_suffix f ".ml" -> fname := Some f
  | _ -> usage ()

let () = Arg.parse [] set_file usage_msg

let fname = match !fname with None -> usage () | Some f -> f

(* Build the Gospel AST from the input file *)
let gospel_ast =
  let c = open_in fname in
  let lb = Lexing.from_channel c in
  Location.init lb fname;
  let ocaml_str_ast = G.Parser_frontend.parse_ocaml_structure_lb lb in
  let mod_name =
    let f = Filename.basename fname in
    String.capitalize_ascii (Filename.chop_extension f)
  in
  let gospel_ast = G.Parser_frontend.parse_structure_gospel ~filename:fname
      ocaml_str_ast mod_name in
  gospel_ast
   
   
let rec term_to_expression t = 
  match t.term_desc with
  | Ttrue                                   -> 
      let f = Longident.Lident "true" in
      let f_sed = Sexp_construct (Location.mkloc f t.term_loc, None) in
      {spexp_desc = f_sed; spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []}
  | Tfalse                                  -> 
      let f = Longident.Lident "false" in
      let f_sed = Sexp_construct (Location.mkloc f t.term_loc, None) in
      {spexp_desc = f_sed; spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []}
  | Tconst c                                -> 
      let sed_c = Sexp_constant c in
      {spexp_desc = sed_c; spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []}
  | Tpreid q                                -> 
    (match q with
    | Qpreid qp ->
      let v = Longident.Lident (qp.pid_str)  in
      let sed_v = Sexp_ident (Location.mkloc v qp.pid_loc) in
      {spexp_desc = sed_v; spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []}
    | Qdot _    -> raise Not_Supported)
  | Tidapp _                                -> raise Not_Supported
  | Tfield _                                -> raise Not_Supported
  | Tapply _                                -> raise Not_Supported
  | Tinfix (t1, pid, t2)                    -> 
    let e3 = term_to_expression t2 in
    let e2 = term_to_expression t1 in
    let op = Longident.Lident (List.nth (String.split_on_char ' ' pid.pid_str) 1) in
    let sed_op = Sexp_ident (Location.mkloc op pid.pid_loc) in
    let e1 = {spexp_desc = sed_op; spexp_loc = pid.pid_loc; spexp_loc_stack = []; spexp_attributes = []} in
    {spexp_desc = Sexp_apply (e1, [(Nolabel, e2); (Nolabel, e3)]); spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []}
  | Tbinop _                                -> raise Not_Supported
  | Tnot t1                                 -> 
    let t_sexp  = term_to_expression t1 in
    let n       = Longident.Lident "not" in
    let n_sed   = Sexp_ident (Location.mkloc n t.term_loc) in
    let e1      = {spexp_desc = n_sed; spexp_loc = t.term_loc; spexp_loc_stack = []; spexp_attributes = []} in
    {spexp_desc = Sexp_apply (e1, [(Nolabel, t_sexp)]); spexp_loc = {loc_start = t.term_loc.loc_start; loc_end = t_sexp.spexp_loc.loc_end; loc_ghost = t.term_loc.loc_ghost}; spexp_loc_stack = []; spexp_attributes = []}
  | Tif _                                   -> raise Not_Supported
  | Tquant _                                -> raise Not_Supported
  | Tattr _                                 -> raise Not_Supported
  | Tlet _                                  -> raise Not_Supported
  | Tcase _                                 -> raise Not_Supported
  | Tcast _                                 -> raise Not_Supported
  | Ttuple _                                -> raise Not_Supported
  | Trecord _                               -> raise Not_Supported
  | Tupdate _                               -> raise Not_Supported
  | Tscope _                                -> raise Not_Supported
  | Told _                                  -> 
      (* tenho que adicionar uma variavel ao inicio para guardar o valor inicial de term *)
      raise Not_Supported

let updated_expression exp pre post =
  let _ (* pre *)   = List.map term_to_expression pre in
  let _ (* post *)  = List.map term_to_expression post in
  exp



let update_svb (svbl : G.Uast.s_value_binding list) =
  let update_svb_item svb =
    match svb.spvb_vspec with
    | None        -> svb
    | Some vspec  ->
        let spvb_exp = updated_expression svb.spvb_expr vspec.sp_pre vspec.sp_post in
        {spvb_pat = svb.spvb_pat; spvb_expr = spvb_exp; spvb_attributes = svb.spvb_attributes; spvb_vspec = (* None *) svb.spvb_vspec; spvb_loc = svb.spvb_loc}
  in
  List.map update_svb_item svbl


let update_ast_item (ssi : G.Uast.s_structure_item) =
  match ssi.sstr_desc with
  | Str_value (rec_flag, svb) -> 
      let svb = update_svb svb in
      {sstr_desc = Str_value (rec_flag, svb); sstr_loc = ssi.sstr_loc}
  | _ -> ssi


let update_ast =
  List.map update_ast_item

(* main entry point *)
let () =
  let _ (* updated_ast *) = update_ast gospel_ast in ()
