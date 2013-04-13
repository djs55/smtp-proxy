open Core.Std
open Async.Std

open Smtp

module Fd = Unix.Fd
module Inet_addr = Unix.Inet_addr
module Socket = Unix.Socket

let port = ref 8025

let stdout_writer = Lazy.force Writer.stdout

let rec dump mails =
  Pipe.read mails
  >>= function
  | `Ok (envelope, body) ->
    Writer.write stdout_writer (Envelope.to_debug_string envelope ^ "\n");
    Writer.transfer stdout_writer body (fun x -> Writer.write stdout_writer (x ^ "\n"))
    >>= fun () ->
    Writer.write stdout_writer "\n";
    dump mails
  | `Eof ->
    return ()

let server () =
  Tcp.Server.create (Tcp.on_port !port)
    (fun _ reader writer ->
      let mails, d = Smtp_core.Server.handler reader writer in
      (* The earliest we could close the connection is when 'd' is determined
         but it's safe to defer closing until the mails have been processed: *)
      dump mails
    )

let () =
  (* TODO: parse arguments *)
  let _ = server () in (* ISTR I should use a wrapper fun? *)
  never_returns (Scheduler.go ())
