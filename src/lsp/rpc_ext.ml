
module Command = struct
  open Protocol
  open Protocol_ext

  type t =
    | MerlinDestruct of DestructCommand.arg
    | UnknownCommand of string * Yojson.Safe.t

  let parse command =
    let open Utils.Result.Infix in
    let parse_yojson f v =
      match f v with
      | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure msg, _) ->
        Error msg
      | r -> Ok r
    in
   match command.ExecuteCommand.command with
   | "merlin/destruct" ->
       parse_yojson DestructCommand.arg_of_yojson command.arguments
        >>| fun params -> MerlinDestruct (params)
   | name ->
       Ok (UnknownCommand (name, command.arguments))
end
