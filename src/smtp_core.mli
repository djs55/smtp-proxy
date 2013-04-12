open Core.Std
open Async.Std

open Smtp

module Mail : sig
  type t = Envelope.t * string Pipe.Reader.t
end


module Server : sig
  val handler: (Mail.t -> unit) -> Reader.t -> Writer.t -> unit Deferred.t
end
