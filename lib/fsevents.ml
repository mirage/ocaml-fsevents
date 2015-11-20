(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Ctypes

module Type = Fsevents_types.C(Fsevents_types_detected)
module C = Fsevents_bindings.C(Fsevents_generated)

module CreateFlags = C.CreateFlags

module EventFlags = C.EventFlags

type t = C.t

type callback = C.Callback.t

let watch latency flags f paths =
  let f = C.Callback.to_cstring_typ f in
  let count = List.length paths in
  let m = allocate_n Cf.String.Bytes.typ ~count in
  ignore (List.fold_left (fun p path ->
    p <-@ path;
    p +@ 1
  ) m paths);
  let paths = CArray.from_ptr m count in
  C.create None f None paths C.EventId.Now latency flags

let schedule_with_run_loop = C.schedule_with_run_loop

let start = C.start
