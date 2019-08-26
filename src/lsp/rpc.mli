(**
 * This encodes LSP RPC state machine.
 *)

module Response : sig

  type error = {
    code : int;
    message : string;
  }

end

module Server_response : sig
  open Protocol

  type t =
    | WorkspaceApplyEdit of ApplyEdit.result

end

module Server_request : sig
  open Protocol

  type t =
    | WorkspaceApplyEdit of ApplyEdit.params

end

module Server_notification : sig
  open Protocol

  type t =
    | PublishDiagnostics of PublishDiagnostics.publishDiagnosticsParams
end

module Client_notification : sig
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChange.params
    | Initialized
    | Exit
    | UnknownNotification of string * Yojson.Safe.t
end

module Request : sig
  open Protocol

  type _ t =
    | Shutdown : unit t
    | TextDocumentHover : Hover.params -> Hover.result t
    | TextDocumentDefinition : Definition.params -> Definition.result t
    | TextDocumentTypeDefinition : TypeDefinition.params -> TypeDefinition.result t
    | TextDocumentCompletion : Completion.params -> Completion.result t
    | TextDocumentCodeAction : CodeAction.params -> CodeAction.result t
    | TextDocumentCodeLens : CodeLens.params -> CodeLens.result t
    | TextDocumentRename : Rename.params -> Rename.result t
    | DocumentSymbol : TextDocumentDocumentSymbol.params -> TextDocumentDocumentSymbol.result t
    | DebugEcho : DebugEcho.params -> DebugEcho.result t
    | DebugTextDocumentGet : DebugTextDocumentGet.params -> DebugTextDocumentGet.result t
    | TextDocumentReferences : References.params -> References.result t
    | TextDocumentHighlight : TextDocumentHighlight.params -> TextDocumentHighlight.result t
    | WorkspaceExecuteCommand : Rpc_ext.Command.t -> ExecuteCommand.result t
    | UnknownRequest : string * Yojson.Safe.t -> unit t
end

type t

type 'state handler = {
  on_initialize :
    t
    -> 'state
    -> Protocol.Initialize.params
    -> ('state * Protocol.Initialize.result, string) result;

  on_request :
    'res.
    t
    -> 'state
    -> Protocol.Initialize.client_capabilities
    -> 'res Request.t
    -> ('state * 'res, string) result;

  on_notification :
    t
    -> 'state
    -> Client_notification.t
    -> ('state, string) result;

  on_response :
    t
    -> 'state
    -> int
    -> (Server_response.t, Response.error) result
    -> ('state, string) result
}

val start :
  'state
  -> 'state handler
  -> in_channel
  -> out_channel
  -> unit

val stop : t -> unit

val send_notification : t -> Server_notification.t -> unit
