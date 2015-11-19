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

let (??<) field int32 = Int32.logand field int32 = 0_l

module Type = Fsevents_types.C(Fsevents_types_detected)

module C(F: Cstubs.FOREIGN) = struct

  (* typedef struct __FSEventStream* FSEventStreamRef; *)
  let typ = typedef (ptr void) "FSEventStreamRef"

  module CreateFlags = struct

    open Type.CreateFlags

    type t = {
      use_cf_types : bool;
      no_defer : bool;
      watch_root : bool;
      ignore_self : bool;
      file_events : bool;
      mark_self : bool;
    }

    let empty = {
      use_cf_types = false;
      no_defer = false;
      watch_root = false;
      ignore_self = false;
      file_events = false;
      mark_self = false;
    }

    let use_cf_types_i = Unsigned.UInt32.to_int32 use_cf_types
    let no_defer_i     = Unsigned.UInt32.to_int32 no_defer
    let watch_root_i   = Unsigned.UInt32.to_int32 watch_root
    let ignore_self_i  = Unsigned.UInt32.to_int32 ignore_self
    let file_events_i  = Unsigned.UInt32.to_int32 file_events
    let mark_self_i    = Unsigned.UInt32.to_int32 mark_self

    let (|||) = Int32.logor

    let (??>) flag int32 = if flag then int32 else 0_l

    let to_uint32 {
      none;
      use_cf_types;
      no_defer;
      watch_root;
      ignore_self;
      file_events;
      mark_self;
    } =
      (??> use_cf_types use_cf_types_i) |||
      (??> no_defer     no_defer_i) |||
      (??> watch_root   watch_root_i) |||
      (??> ignore_self  ignore_self_i) |||
      (??> file_events  file_events_i) |||
      (??> mark_self    mark_self_i)

    let of_uint32 i = {
      use_cf_types = ??< i use_cf_types_i;
      no_defer     = ??< i no_defer_i;
      watch_root   = ??< i watch_root_i;
      ignore_self  = ??< i ignore_self_i;
      file_events  = ??< i file_events_i;
      mark_self    = ??< i mark_self_i;
    }

    let typ = view ~read:of_uint32 ~write:to_uint32 t

  end

  module EventFlags = struct

    open Type.EventFlags

    type dropping_party = {
      user : bool;
      kernel: bool;
    }

    type item_type = File | Symlink | Dir | Hardlink

    type t = {
      must_scan_subdirs    : dropping_party option;
      event_ids_wrapped    : bool;
      history_done         : bool;
      root_changed         : bool;
      mount                : bool;
      unmount              : bool;
      own_event            : bool;
      item_created         : bool;
      item_removed         : bool;
      item_inode_meta_mod  : bool;
      item_renamed         : bool;
      item_modified        : bool;
      item_finder_info_mod : bool;
      item_change_owner    : bool;
      item_xattr_mod       : bool;
      item_type            : item_type option;
      item_is_last_hardlink: bool;
    }

    let must_scan_subdirs_i    = Unsigned.UInt32.to_int32 must_scan_subdirs
    let user_dropped_i         = Unsigned.UInt32.to_int32 user_dropped
    let kernel_dropped_i       = Unsigned.UInt32.to_int32 kernel_dropped
    let event_ids_wrapped_i    = Unsigned.UInt32.to_int32 event_ids_wrapped
    let history_done_i         = Unsigned.UInt32.to_int32 history_done
    let root_changed_i         = Unsigned.UInt32.to_int32 root_changed
    let mount_i                = Unsigned.UInt32.to_int32 mount
    let unmount_i              = Unsigned.UInt32.to_int32 unmount
    let own_event_i            = Unsigned.UInt32.to_int32 own_event
    let item_created_i         = Unsigned.UInt32.to_int32 item_created
    let item_removed_i         = Unsigned.UInt32.to_int32 item_removed
    let item_inode_meta_mod_i  = Unsigned.UInt32.to_int32 item_inode_meta_mod
    let item_renamed_i         = Unsigned.UInt32.to_int32 item_renamed
    let item_modified_i        = Unsigned.UInt32.to_int32 item_modified
    let item_finder_info_mod_i = Unsigned.UInt32.to_int32 item_finder_info_mod
    let item_change_owner_i    = Unsigned.UInt32.to_int32 item_change_owner
    let item_xattr_mod_i       = Unsigned.UInt32.to_int32 item_xattr_mod
    let item_is_file_i         = Unsigned.UInt32.to_int32 item_is_file
    let item_is_dir_i          = Unsigned.UInt32.to_int32 item_is_dir
    let item_is_symlink_i      = Unsigned.UInt32.to_int32 item_is_symlink
    let item_is_hardlink_i     = Unsigned.UInt32.to_int32 item_is_hardlink
    let item_is_last_hardlink_i= Unsigned.UInt32.to_int32 item_is_last_hardlink

    let to_uint32 {
      must_scan_subdirs;
      event_ids_wrapped;
      history_done;
      root_changed;
      mount;
      unmount;
      own_event;
      item_created;
      item_removed;
      item_inode_meta_mod;
      item_renamed;
      item_modified;
      item_finder_info_mod;
      item_change_owner;
      item_xattr_mod;
      item_type;
      item_is_last_hardlink;
    } =
      ()

    let must_scan_subdirs_of_uint32 i =
      if ??< i must_scan_subdirs_i
      then Some {
        user = ??< i user_dropped_i;
        kernel = ??< i kernel_dropped_i;
      } else None

    let item_type_of_uint32 i =
      if ??< i item_is_file_i
      then Some File
      else if ??< i item_is_dir_i
      then Some Dir
      else if ??< i item_is_symlink_i
      then Some Symlink
      else if ??< i item_is_hardlink_i
      then Some Hardlink
      else None

    let of_uint32 i = {
      must_scan_subdirs     = must_scan_subdirs_of_uint32 i;
      event_ids_wrapped     = ??< i event_ids_wrapped_i;
      history_done          = ??< i history_done_i;
      root_changed          = ??< i root_changed_i;
      mount                 = ??< i mount_i;
      unmount               = ??< i unmount_i;
      own_event             = ??< i own_event_i;
      item_created          = ??< i item_created_i;
      item_removed          = ??< i item_removed_i;
      item_inode_meta_mod   = ??< i item_inode_meta_mod_i;
      item_renamed          = ??< i item_renamed_i;
      item_modified         = ??< i item_modified_i;
      item_finder_info_mod  = ??< i item_finder_info_mod_i;
      item_change_owner     = ??< i item_change_owner_i;
      item_xattr_mod        = ??< i item_xattr_mod_i;
      item_type             = item_type_of_uint32 i;
      item_is_last_hardlink = ??< i item_is_last_hardlink;
    }

    let typ = view ~read:of_uint32 ~write:to_uint32 t

  end

  module EventId = struct

    type t =
      | Now
      | Since of int64

    let of_uint64 i =
      if i = Type.EventId.since_now
      then Now
      else Since (Unsigned.UInt64.to_int64 i)

    let to_uint64 = function
      | Now -> Type.EventId.since_now
      | Since i -> Unsigned.UInt64.of_int64 i

    let typ = view ~read:of_uint64 ~write:to_uint64 Type.EventId.t

  end

  module Callback = struct

    type t = string -> EventFlags.t -> int64 -> unit

    (* typedef void ( *FSEventStreamCallback )(
         ConstFSEventStreamRef streamRef,
         void *clientCallBackInfo,
         size_t numEvents,
         void *eventPaths,
         const FSEventStreamEventFlags eventFlags[],
         const FSEventStreamEventId eventIds[]);
    *)
    let cstring_typ = Foreign.funptr (
      typ @->
      ptr void @->
      size_t @->
      ptr string @->
      ptr EventFlags.t @->
      ptr Type.EventId.t @->
      returning void
    )

    let to_cstring_typ fn _stream _info num_events paths flags ids =
      let n = Unsigned.Size_t.to_int num_events in
      let paths = CArray.from_ptr paths n in
      let flags = CArray.from_ptr flags n in
      let ids   = CArray.from_ptr ids   n in
      for i = 0 to n - 1 do
        fn (Array.get paths i) (Array.get flags i) (Array.get ids i)
      done

  end

  module Context = struct

    type 'a t = {
      version : int;
      info : 'a;
      retain : Cf.Allocate.retain_callback_t;
      release : Cf.Allocate.release_callback_t;
      copy_description : Cf.Allocate.copy_description_t;
    }

  end

  module PathArray = Cf.Array.Make(Cf.String)

  (* extern FSEventStreamRef FSEventStreamCreate(
       CFAllocatorRef allocator,
       FSEventStreamCallback callback,
       FSEventStreamContext *context,
       CFArrayRef pathsToWatch,
       FSEventStreamEventId sinceWhen,
       CFTimeInterval latency,
       FSEventStreamCreateFlags flags
     ); *)
  let create = F.foreign "FSEventStreamCreate" (
    ptr_opt void @->
    Callback.cstring_typ @->
    ptr_opt context @->
    PathArray.typ @->
    EventId.typ @->
    Cf.TimeInterval.typ @->
    CreateFlags.typ @->
    returning typ
  )

end
