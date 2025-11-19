(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

(** In-memory store.

    A simple in-memory store, using hash tables. Once one of the functors below
    is instantiated to a module [M], it has a unique shared hash-table: multiple
    invocation of [M.create] will see and manipulate the same contents. *)

module Conf : sig
  val spec : Irmin.Backend.Conf.Spec.t
end

val config : unit -> Irmin.config
(** Configuration values. *)

module Append_only : Irmin.Append_only.Maker
(** An in-memory store for append-only values. *)

module Content_addressable : Irmin.Content_addressable.Maker
(** An in-memory store for content-addressable values. *)

module Atomic_write : Irmin.Atomic_write.Maker
(** An in-memory store with atomic-write guarantees. *)

(** Functor for building in-memory KV stores.

    This functor builds a key-value store that stores its data in memory. It is
    parameterized by the type of the contents stored in the database. In the
    example below, we use [Irmin.Contents.String], which means that the values
    are strings.

    For example, to create a store with string contents:
{[
  (* Create a new in-memory store with string contents. [Irmin.Contents.String]
     is a pre-defined module that uses strings as values. *)
  module Store = Irmin_mem.KV (Irmin.Contents.String)

  let main =
    (* Create a new repository. This is a persistent handle to the store. *)
    let* repo = Store.Repo.v (Irmin_mem.config ()) in
    (* Get the main branch of the repository. Irmin is git-like, so it has
       branches. *)
    let* main = Store.main repo in
    (* Set a value at path ["a"; "b"] to "c". This creates a new commit. The
       ~info argument is used to set the commit message. *)
    let* () = Store.set_exn main ~info:(Irmin.Info.v "commit 1") ["a"; "b"] "c" in
    (* Get the value at path ["a"; "b"]. *)
    let* str = Store.get main ["a"; "b"] in
    print_endline str
]}
*)
module KV :
  Irmin.KV_maker
    with type endpoint = unit
     and type metadata = unit
     and type info = Irmin.Info.default

include Irmin.Maker with type endpoint = unit
(** Constructor for in-memory Irmin store. *)
