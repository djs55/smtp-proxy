open Core.Std
open Async.Std

open Smtp

module Mail : sig
  type t = Envelope.t * string Pipe.Reader.t
end


module Server : sig
  val handler: Reader.t -> Writer.t -> Mail.t Pipe.Reader.t * unit Deferred.t
end
