(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let (/) = Filename.concat

module EventFlags : Alcotest.TESTABLE = struct

  type t = Fsevents.EventFlags.t

  let pp fmt flags =
    Format.pp_print_string fmt (Fsevents.EventFlags.to_string_one_line flags)

  let equal flags flags' = flags = flags'

end

let event_flags = (module EventFlags : Alcotest.TESTABLE)

let create_flags = Fsevents.CreateFlags.detailed_interactive

let run_loop_mode = Cf.RunLoop.Mode.Default

let () = Random.self_init ()

let rec mktmpdir_under dir =
  let k = Random.int 0x10000 in
  let path = dir / (Printf.sprintf "fsevents_test_%04x" k) in
  try Unix.mkdir path 0o700; path
  with Unix.Unix_error (Unix.EEXIST, _, _) -> mktmpdir_under dir

let ensuredir dir =
  try Unix.mkdir dir 0o700; dir
  with Unix.Unix_error (Unix.EEXIST, _, _) -> dir

let with_temp_stream f () =
  let tmp = ensuredir (Unix.getcwd () / "tmp") in
  let dir = mktmpdir_under tmp in
  let watcher = Fsevents_lwt.watch 0. create_flags [dir] in
  let { Fsevents_lwt.event_stream; stream; } = watcher in
  Lwt.(async (fun () ->
    Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents.schedule_with_run_loop event_stream runloop run_loop_mode;
      if not (Fsevents.start event_stream)
      then print_endline "failed to start FSEvents stream"
    )
  ));
  Lwt_main.run (f dir stream);
  Unix.(match system ("rm -r "^dir) with
    | WEXITED 0 -> ()
    | _ -> Alcotest.fail "couldn't delete test dir"
  )

module Event = struct
  open Lwt
  open Fsevents.EventFlags
  open Fsevents_lwt

  type t =
    | DirType of t
    | Create of string

  let fail_event remaining event_type =
    Alcotest.fail
      ("expected "^event_type^" event but current event is:\n"^
       remaining.path^"\n"^
       to_string_one_line remaining.flags)

  let has_create remaining =
    if remaining.flags.item_created
    then
      { remaining with flags = { remaining.flags with item_created = false } }
    else fail_event remaining "ItemCreated"

  let has_dir_type remaining = match remaining.flags.item_type with
    | Some Dir ->
      { remaining with flags = { remaining.flags with item_type = None } }
    | _ -> fail_event remaining "ItemType(Dir)"

  let check_path { path; flags } expected_path =
    if expected_path = path
    then ()
    else
      Alcotest.fail
        ("unexpected path "^path^" expecting "^expected_path^"\n"^
         "for event "^to_string_one_line flags^"\n")

  let rec check_one remaining = function
    | Create path -> check_path remaining path; has_create remaining
    | DirType n -> check_one (has_dir_type remaining) n

  let rec check ?remaining stream = function
    | [] -> Lwt.return_unit
    | first::rest ->
      (match remaining with
       | Some { flags } when flags = zero -> Lwt_stream.next stream
       | Some event -> return event
       | None -> Lwt_stream.next stream
      ) >>= fun remaining ->
      check ~remaining:(check_one remaining first) stream rest
end

module FileMod = struct
  open Lwt

  let create dir stream =
    let path = dir / "create_file_test" in
    Lwt_unix.(openfile path [O_CREAT] 0o600)
    >>= fun fd ->
    Lwt_unix.close fd
    >>= fun () ->
    Event.(check stream [DirType (Create dir); Create path])

  let create_delete dir stream = Lwt.return_unit

  let tests = [
    "create",        `Quick, with_temp_stream create;
    "create_delete", `Quick, with_temp_stream create_delete;
  ]
end

let tests = [
  "FileMod", FileMod.tests;
]

;;
Alcotest.run "FSEvents" tests
