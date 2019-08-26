(** Merlin specific protocol *)
open Protocol

module DestructCommand = struct

  type arg = {
    textDocument: TextDocumentIdentifier.t;
    range: range;
  }
  [@@deriving_inline yojson]

  
let _ = fun (_ : arg) -> ()
let arg_of_yojson =
  (let _tp_loc = "src/lsp/protocol_ext.ml.DestructCommand.arg" in
   function
   | `Assoc field_yojsons as yojson ->
       let textDocument_field = ref None
       and range_field = ref None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "textDocument" ->
                   (match Ppx_yojson_conv_lib.(!) textDocument_field with
                    | None ->
                        let fvalue =
                          TextDocumentIdentifier.t_of_yojson _field_yojson in
                        textDocument_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "range" ->
                   (match Ppx_yojson_conv_lib.(!) range_field with
                    | None ->
                        let fvalue = range_of_yojson _field_yojson in
                        range_field := (Some fvalue)
                    | Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) textDocument_field),
                           (Ppx_yojson_conv_lib.(!) range_field))
                   with
                   | (Some textDocument_value, Some range_value) ->
                       {
                         textDocument = textDocument_value;
                         range = range_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) textDocument_field)
                              None), "textDocument");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) range_field) None),
                           "range")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> arg)
let _ = arg_of_yojson
let yojson_of_arg =
  (function
   | { textDocument = v_textDocument; range = v_range } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds = let arg = yojson_of_range v_range in ("range", arg) :: bnds in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
         ("textDocument", arg) :: bnds in
       `Assoc bnds : arg -> Ppx_yojson_conv_lib.Yojson.Safe.t)
let _ = yojson_of_arg
[@@@end]
end
