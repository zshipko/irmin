(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
module Graphql (S : Irmin.S) =
  Irmin_unix.Graphql.Server.Make (S) (Irmin_unix.Graphql.Server.Remote.None)

module Make (S : Irmin.S) = struct
  module Graphql = Graphql (S)

  let schema = Graphql.schema

  let execute schema = Irmin_graphql.Server.Schema.execute schema ()
end

let query s =
  match Graphql_parser.parse ("query {" ^ s ^ "}") with
  | Ok d -> d
  | Error e -> failwith ("Invalid query: " ^ e)

let mutation s =
  match Graphql_parser.parse ("mutation {" ^ s ^ "}") with
  | Ok d -> d
  | Error e -> failwith ("Invalid mutation: " ^ e)

let variables = None

let operation_name = None

let response f = function
  | Ok (`Response json) -> f json
  | Ok (`Stream _) -> Alcotest.fail "Invalid response"
  | Error json -> Alcotest.fail (Yojson.Basic.to_string json)

let json = Alcotest.testable Yojson.Basic.pp Yojson.Basic.equal

let test_empty execute =
  let q = query {| master { head { hash } } |} in
  execute ?variables q
  >|= response (fun j ->
          let j =
            Yojson.Basic.Util.(
              member "data" j |> member "master" |> member "head")
          in
          Alcotest.(check json) "Empty head" `Null j )

let test_set execute =
  let q = mutation {| set(key: "a/b/c", value: "123") { hash } |} in
  execute ?variables q
  >|= response (fun j ->
          let j =
            Yojson.Basic.Util.(
              member "data" j |> member "set" |> member "hash")
          in
          Alcotest.(check bool)
            "New hash" true
            (not (Yojson.Basic.equal `Null j)) )

let test_get execute =
  let q = query {| master { head { tree { get(key: "a/b/c") } } } |} in
  execute ?variables q
  >|= response (fun j ->
          let j =
            Yojson.Basic.Util.(
              member "data" j |> member "master" |> member "head"
              |> member "tree" |> member "get")
          in
          Alcotest.check json "Get" (`String "123") j )

let test (module S : Irmin_test.S) =
  let module T = Make (S) in
  Irmin_git.config "/tmp/irmin-graphql-test" |> S.Repo.v >|= fun repo ->
  let schema = T.schema repo in
  let execute = T.execute schema in
  let wrap f () = Lwt_main.run (f (execute ?operation_name)) in
  [ ("Empty", `Quick, wrap test_empty);
    ("Set", `Quick, wrap test_set);
    ("Get", `Quick, wrap test_get)
  ]

let tests = [ Test_git.suite; Test_mem.suite ]

let run t =
  let name = Printf.sprintf "GRAPHQL.%s" t.Irmin_test.name in
  test t.store >|= fun test -> (name, test)

let suites tests =
  Alcotest.run "irmin-graphql"
    (Lwt_main.run
       (Lwt_list.fold_left_s
          (fun acc x -> run x >|= fun y -> y :: acc)
          [] tests))

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Irmin_test.reporter ())
