
module Request = struct
  type t =
    | Helo of string
    | MailFrom of string
    | RcptTo of string
    | Data
    | Quit
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

end
