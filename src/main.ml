open Core.Std
open Async.Std

open Smtp

module Fd = Unix.Fd
module Inet_addr = Unix.Inet_addr
module Socket = Unix.Socket


let port = ref 8025

let server () =
  Tcp.Server.create (Tcp.on_port !port)
    (fun _ reader writer ->
      Smtp_core.Server.handler (fun _ -> ()) reader writer
    )

let () =
  (* TODO: parse arguments *)
  let _ = server () in (* ISTR I should use a wrapper fun? *)
  never_returns (Scheduler.go ())
