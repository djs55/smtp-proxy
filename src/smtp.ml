
(* [suffix prefix x] returns [None] if [prefix] is not a case-insensitive
   prefix of [x] and [Some suffix] if [prefix ^ suffix = x] *)
let suffix prefix x =
  let prefix' = String.length prefix and x' = String.length x in
  if prefix' <= x' && (String.lowercase (String.sub x 0 prefix') = String.lowercase prefix)
  then Some (String.sub x prefix' (x' - prefix'))
  else None

module Request = struct
  type t =
    | Helo of string
    | MailFrom of string
    | RcptTo of string
    | Data
    | Quit
    | Unknown

  let of_string x =
    match suffix "HELO" x with
    | Some x -> Helo x
    | None ->
      begin match suffix "MAIL FROM:" x with
      | Some x -> MailFrom x
      | None ->
        begin match suffix "RCPT TO:" x with
        | Some x -> RcptTo x
        | None ->
          begin match suffix "DATA" x with
            | Some "" -> Data
            | Some _ -> Unknown
            | None ->
              begin match suffix "QUIT" x with
              | Some "" -> Quit
              | Some _
              | None -> Unknown
              end
          end
        end
      end
end

module Response = struct
  type code =
    | Ok
    | PleaseSendBody
    | BusyRetryLater
    | SyntaxError
    | AccessForbidden
    | UnknownError

  let code_of_int = function
    | 250 -> Ok
    | 354 -> PleaseSendBody
    | 450 -> BusyRetryLater
    | 500 -> SyntaxError
    | 550 -> AccessForbidden
    | 554
    | _   -> UnknownError (* treat everything else as 'unknown' *)

  let int_of_code = function
    | Ok              -> 250
    | PleaseSendBody  -> 354
    | BusyRetryLater  -> 450
    | SyntaxError     -> 500
    | AccessForbidden -> 550
    | UnknownError    -> 554

  type t = code * string

  let ok = Ok, "ok"
end

module Envelope = struct
  type t = {
    return_path: string;
    recipient_addresses: string list;
  }

  open Request
  let update t = function
    | MailFrom x -> {t with return_path = x}
    | RcptTo x -> {t with recipient_addresses = x :: t.recipient_addresses}
    | _ -> t
end
