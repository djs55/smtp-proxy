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
      let send t = Writer.write writer (Response.to_string t) in
      Deferred.create (fun finished ->
      send (Response.ServiceReady, "ocaml smtp-proxy");

        let rec loop envelope =
          upon (Reader.read_line reader) (function
          | `Ok line ->
            let open Request in
            let req = of_string line in
            begin match req with
            | Unknown -> send (Response.SyntaxError, "I have no idea what you just said")
            | _ -> send Response.ok
            end;
            let envelope = Envelope.update envelope req in

            message (sprintf "Server got query: %s\n" line);
            Writer.write writer (sprintf "envelope = %s\n" (Envelope.to_debug_string envelope));
            loop envelope
          | `Eof ->
            message "Server got EOF\n")
        in
        loop Envelope.empty))

let () =
  (* TODO: parse arguments *)
  let _ = server () in (* ISTR I should use a wrapper fun? *)
  never_returns (Scheduler.go ())
