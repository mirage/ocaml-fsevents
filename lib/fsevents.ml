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

module EventFlags = struct
  include C.EventFlags

  let string_of_must_scan_subdirs = function
    | Some { user = true; kernel = true } -> "user+kernel"
    | Some { user = true } -> "user"
    | Some { kernel = true } -> "kernel"
    | Some { user = false; kernel = false } -> "unknown"
    | None -> "false"

  let string_of_item_type = function
    | None -> "false"
    | Some File -> "file"
    | Some Dir -> "dir"
    | Some Symlink -> "symlink"
    | Some Hardlink -> "hardlink"

  let to_string t =
    Printf.sprintf
      "{\n  must_scan_subdirs     = %s;\
        \n  event_ids_wrapped     = %b;\
        \n  history_done          = %b;\
        \n  root_changed          = %b;\
        \n  mount                 = %b;\
        \n  unmount               = %b;\
        \n  own_event             = %b;\
        \n  item_created          = %b;\
        \n  item_removed          = %b;\
        \n  item_inode_meta_mod   = %b;\
        \n  item_renamed          = %b;\
        \n  item_modified         = %b;\
        \n  item_finder_info_mod  = %b;\
        \n  item_change_owner     = %b;\
        \n  item_xattr_mod        = %b;\
        \n  item_type             = %s;\
        \n  item_is_last_hardlink = %b;\
        \n}"
      (string_of_must_scan_subdirs t.must_scan_subdirs)
      t.event_ids_wrapped
      t.history_done
      t.root_changed
      t.mount
      t.unmount
      t.own_event
      t.item_created
      t.item_removed
      t.item_inode_meta_mod
      t.item_renamed
      t.item_modified
      t.item_finder_info_mod
      t.item_change_owner
      t.item_xattr_mod
      (string_of_item_type t.item_type)
      t.item_is_last_hardlink

end

type t = C.t

type callback = C.Callback.t

let watch latency flags f paths =
  let f = C.Callback.to_cstring_typ f in
  C.create None f None paths C.EventId.Now latency flags

let schedule_with_run_loop = C.schedule_with_run_loop

let start = C.start
