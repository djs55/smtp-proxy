open Core.Std
open Async.Std

open Smtp

module Fd = Unix.Fd
module Inet_addr = Unix.Inet_addr
module Socket = Unix.Socket

let stdout_writer = Lazy.force Writer.stdout
let message s = Writer.write stdout_writer s

let port = ref 8025

let server () =
  Tcp.Server.create (Tcp.on_port !port)
    (fun _ reader writer ->
      Deferred.create (fun finished ->
        let rec loop () =
          upon (Reader.read_line reader) (function
          | `Ok query ->
            message (sprintf "Server got query: %s\n" query);
            Writer.write writer (sprintf "Response to %s\n" query);
            loop ()
          | `Eof ->
            message "Server got EOF\n")
        in
        loop ()))

let () =
  (* TODO: parse arguments *)
  let _ = server () in (* ISTR I should use a wrapper fun? *)
  never_returns (Scheduler.go ())
