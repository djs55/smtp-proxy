
module Request : sig 
  type t =
    | Helo of string
    | MailFrom of string
    | RcptTo of string
    | Data
    | Quit
    | Unknown (** something we didn't understand *)
  (** requests sent by the client to the server *)

  val of_string: string -> t
end

module Response : sig
  type code =
    | ServiceReady
    | Ok
    | PleaseSendBody
    | BusyRetryLater
    | SyntaxError
    | AccessForbidden
    | UnknownError

  type t = code * string
  (** reply sent by the server to the client consisting of a
      semantically-meaningful code plus a human-readable string. *)

  val to_string: t -> string

  val ok: t
end

module Envelope : sig
  type t

  val empty: t

  val update: t -> Request.t -> t

  val to_debug_string: t -> string
end
