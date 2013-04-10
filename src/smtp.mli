
module Request : sig 
  type t =
    | Helo of string
    | MailFrom of string
    | RcptTo of string
    | Data
    | Quit
  (** requests sent by the client to the server *)

end

module Response : sig
  type code =
    | Ok
    | PleaseSendBody
    | BusyRetryLater
    | SyntaxError
    | AccessForbidden
    | UnknownError

  type t = code * string
  (** reply sent by the server to the client consisting of a
      semantically-meaningful code plus a human-readable string. *)
end

