open Core.Std
open Async.Std

open Smtp

module Mail = struct
  type t = Envelope.t * string Pipe.Reader.t
end


let stdout_writer = Lazy.force Writer.stdout
let message s = Writer.write stdout_writer s

module Server = struct

  let handler callback reader writer =
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
        message "Server got EOF during body\n";
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
          callback (envelope, reader);
          body envelope writer
        | Quit ->
          send Response.ok;
          message "client said quit\n";
          return ()
        | _ ->
          let envelope = Envelope.update envelope req in
          send Response.ok;
          message (sprintf "Server got query: %s\n" line);
          message (sprintf "envelope = %s\n" (Envelope.to_debug_string envelope));
          loop envelope
        end;
      | `Eof ->
        message "Server got EOF\n";
        return () in
    loop Envelope.empty

end
