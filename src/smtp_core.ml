open Core.Std
open Async.Std

open Smtp

module Mail = struct
  type t = Envelope.t * string Pipe.Reader.t
end


let stdout_writer = Lazy.force Writer.stdout
let log s = Writer.write stdout_writer s

module Server = struct

  let handler reader writer =
    let mails_reader, mails_writer = Pipe.create () in
    let send t = Writer.write writer (Response.to_string t) in
    send (Response.ServiceReady, "ocaml smtp-proxy");
    let rec body envelope writer : unit Deferred.t =
      Reader.read_line reader
      >>= function
      | `Ok "." ->
        send Response.ok;
        Pipe.close writer;
        loop Envelope.empty
      | `Ok x ->
        Pipe.write_without_pushback writer x;
        body envelope writer
      | `Eof ->
        log "Server got EOF during body\n";
        (* XXX: We probably should abort the processing of this mail *)
        Pipe.close writer;
        return ()

    and loop envelope : unit Deferred.t =
      Reader.read_line reader
      >>= function
      | `Ok line ->
        let open Request in
        let req = of_string line in
        begin match req with
        | Unknown ->
          send (Response.SyntaxError, "I have no idea what you just said");
          loop envelope
        | Data ->
          send (Response.PleaseSendBody, "Start mail input; end with <CR><LF>.<CR><LF>");
          let reader, writer = Pipe.create () in
          let mail = envelope, reader in
          Pipe.write_without_pushback mails_writer mail;
          body envelope writer
        | Quit ->
          send Response.ok;
          Pipe.close mails_writer;
          return ()
        | _ ->
          let envelope = Envelope.update envelope req in
          send Response.ok;
          loop envelope
        end;
      | `Eof ->
        log "Server got EOF\n";
        Pipe.close mails_writer;
        return () in
    mails_reader, loop Envelope.empty

end
