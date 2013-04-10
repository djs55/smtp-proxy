open Core.Std
open Async.Std

let () = never_returns (Scheduler.go ())
