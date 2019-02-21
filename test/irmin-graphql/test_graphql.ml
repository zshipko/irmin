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

module Store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Disk = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Client =
  Irmin_unix.Graphql.Client.Make
    (Irmin_git.Metadata)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)


(*let init_test_repo branch =
  let info = Irmin_unix.info in
  let test_repo_cfg = Irmin_git.config "_test_repo" in
  Disk.Repo.v test_repo_cfg >>= fun repo -> Disk.of_branch repo branch >>= fun t ->
  Disk.set_exn t ~info:(info "a") ["a"] (branch ^ "1") >>= fun () ->
  Disk.set_exn t ~info:(info "b") ["b"] (branch ^ "2") >>= fun () ->
  Disk.set_exn t ~info:(info "c") ["c"] (branch ^ "3") >>= fun () ->
  Disk.set_exn t ~info:(info "d") ["d"] (branch ^ "4") >>= fun () -> Lwt.return_unit *)

(* See https://github.com/mirage/ocaml-cohttp/issues/511 *)
let () = Lwt.async_exception_hook := (fun e ->
    Fmt.pr "Async exception caught: %a" Fmt.exn e;
  )

let ( / ) = Filename.concat
let pid_file = Filename.get_temp_dir_name () / "irmin-graphql-test.pid"

let rec wait_for_the_server_to_start () =
  if Sys.file_exists pid_file then (
    let ic = open_in pid_file in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Logs.debug (fun l -> l "read PID %d from %s" pid pid_file);
    Unix.unlink pid_file;
    Unix.sleep 1;
    pid
  ) else (
    Logs.debug (fun l -> l "waiting for the server to start...");
    Unix.sleep 1;
    wait_for_the_server_to_start ()
  )

let unwrap = function
  | Ok x -> Lwt.return x
  | Error (`Msg e) -> Alcotest.fail e

let check_type_eq name t a b =
  Alcotest.(check bool) name true (Irmin.Type.equal t a b)

let check_type_not_eq name t a b =
  Alcotest.(check bool) name false (Irmin.Type.equal t a b)

(*let test_set_get branch client =
  Client.set ~branch client ["a"; "b"; "c"] "123" >>= unwrap >>= fun _ ->
  Client.get ~branch client ["a"; "b"; "c"] >>= unwrap >>= fun s ->
  Client.set ~branch client ["foo"] "bar" >>= unwrap >>= fun _ ->
  Client.get ~branch client ["foo"] >>= unwrap >|= fun s' ->
  Alcotest.(check string) "get a/b/c" "123" s;
  Alcotest.(check string) "get foo" "bar" s'

let test_head branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  Client.commit_info client hash >>= unwrap >>= fun info' ->
  check_type_eq "Commit info" Irmin.Info.t info.info info'.info;
  Client.set ~branch client ["foo"] "baz" >>= unwrap >|= fun hash' ->
  check_type_not_eq "hash after set" Store.Hash.t hash hash'

let test_remove branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  Client.remove client ["foo"] >>= unwrap >>= fun hash' ->
  check_type_not_eq "hash after remove" Store.Hash.t hash hash';
  Client.find client ["foo"] >>= unwrap >|= function
  | Some _ -> Alcotest.fail "foo should be empty"
  | None -> ()

let test_tree branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  let tree = Store.Tree.empty in
  Store.Tree.add tree ["test"; "a"] "1" >>= fun tree ->
  Store.Tree.add tree ["test"; "b"] "2" >>= fun tree ->
  Store.Tree.add tree ["test"; "c"] "3" >>= fun tree ->
  Client.update_tree ~branch client [] tree >>= unwrap >>= fun hash' ->
  Store.Tree.get_tree tree  ["test"] >>= fun tree ->
  check_type_not_eq "hash after update tree" Store.Hash.t hash hash';
  Client.get_tree ~branch client ["test"] >>= unwrap >>= fun tree' ->
  check_type_eq "tree after update tree" Store.tree_t tree tree';
  Store.Tree.remove tree ["a"] >>= fun tree ->
  Client.set_tree ~branch client ["test"] tree >>= unwrap >>= fun _ ->
  Client.get_tree ~branch client ["test"] >>= unwrap >>= fun tree' ->
  Store.Tree.mem tree' ["a"] >|= fun exists ->
  Alcotest.(check bool) "test/a removed" false exists

let test_merge branch client =
  Client.set ~branch:"aaa" client ["test-merge"] "abc" >>= fun _ ->
  Client.merge client ~into:branch "aaa"  >>= fun _ ->
  Client.get client ~branch ["test-merge"] >|= function
  | Ok x ->
    Alcotest.(check string) "merge" "abc" x
  | Error (`Msg msg) -> Alcotest.fail msg

let repo_path =
  let p = Unix.getcwd () / "_test_repo" in
  "file://" ^ p

let test_clone branch client =
  (Client.clone ~branch client repo_path >>= function
  | Ok _ -> Lwt.return_unit
  | Error (`Msg msg) -> Alcotest.failf "clone: %s" msg)

let test_pull branch client =
  (* See https://github.com/mirage/irmin/issues/600 *)
  if branch <> "master" then Lwt.return_unit
  else
    (Client.pull ~branch client repo_path >>= function
    | Ok _ -> Lwt.return_unit
    | Error (`Msg msg) -> Alcotest.failf "pull: %s" msg) >>= fun () ->
    Client.get ~branch client ["d"] >|= function
    | Ok x -> Alcotest.(check string) "after pull" (branch ^ "4") x
    | Error (`Msg msg) -> Alcotest.failf "after pull error: %s" msg

let test_set_get_all branch client =
  let key = ["x"] in
  let value = "testing" in
  Client.set_all client ~branch key value `Everybody >>= function
  | Ok _ ->
    (Client.get_all client ~branch key >|= function
      | Ok (v, m) ->
        Alcotest.(check bool) "values equal" true (String.equal v value);
        Alcotest.(check bool) "metadata equal" true (m = `Everybody)
      | Error (`Msg msg) -> Alcotest.failf "get_all: %s" msg)
  | Error (`Msg msg) -> Alcotest.failf "set_all: %s" msg

let test_branches branch client =
  let l = ["aaa"; "master"]  in
  let l = if not (List.mem branch l) then branch :: l else l in
  let l = List.sort String.compare l in
  Client.branches client >|= function
  | Ok branches ->
    let l' =
      List.sort String.compare branches
    in
    Alcotest.(check (list string)) "Branches" l l'
  | Error (`Msg msg) -> Alcotest.failf "branches: %s" msg

let test_snapshot_revert branch client =
  Client.set client ~author:"AAA"  ~message:"BBB" ~branch ["something"] "abc" >>= function
  | Ok hash ->
    (Client.set client ~branch ["something"] "xyz" >>= fun _ ->
     Client.revert client ~branch hash >>= fun _ ->
     Client.get client ~branch ["something"] >|= function
     | Ok x -> Alcotest.(check string) "Get after revert" "abc" x
     | Error (`Msg msg) -> Alcotest.failf "revert: %s" msg)
  | Error (`Msg msg) ->
    Alcotest.failf "set: %s" msg

let test_lca branch client =
  Client.branch_info client branch >>= function
  | Ok commit ->
    let commit = List.hd commit.parents in
    (Client.lca ~branch client commit >|= function
      | Ok commits ->
        Alcotest.(check bool) "LCA" true
          (List.exists (fun x -> x.Client.hash = commit) commits)
      | Error (`Msg msg) -> Alcotest.failf "branch_info: %s" msg)
  | Error (`Msg msg) -> Alcotest.failf "branch_info: %s" msg

let test_push branch client =
  let remote = "file://" ^ (Unix.getcwd () / "_push") in
  Client.push client ~branch remote >|= function
  | Ok _ -> ()
  | Error (`Msg msg) -> Alcotest.failf "push: %s" msg

let tests = [
  "set/get", `Quick, test_set_get;
  "branch_info/commit_info", `Quick, test_head;
  "remove", `Quick, test_remove;
  "tree", `Quick, test_tree;
  "clone", `Quick, test_clone;
  "pull", `Quick, test_pull;
  "merge", `Quick, test_merge;
  "set_all/get_all", `Quick, test_set_get_all;
  "branches", `Quick, test_branches;
  "snapshot/revert", `Quick, test_snapshot_revert;
  "lca", `Quick, test_lca;
  "push", `Quick, test_push;
]*)

let uri = Uri.of_string "http://localhost:80808/graphql"

(*let run_tests name tests =
  let client = Client.init uri in
  let tests branch =
    List.map (fun (name, speed, f) ->
        let f () =
          ignore (Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0));
          init_test_repo branch >>= fun () ->
          f branch client
        in
        branch ^ ":" ^ name, speed, (fun () -> Lwt_main.run (f ()))
      ) tests
  in
  let a = tests "master" in
  let b = tests "testing" in
  Alcotest.run name [name, a @ b] *)

let server_pid = ref 0

let clean () = Unix.kill !server_pid Sys.sigint

let graphql_store (module S: Irmin_test.S) =
  Irmin_test.store (module Irmin_unix.Graphql.Client.Make) (module S.Metadata)

let suite server =
  let open Irmin_test in
  { name = Printf.sprintf "GRAPHQL.%s" server.name;

    init = begin fun () ->
      let _ = Sys.command (Printf.sprintf "dune exec -- %s server & echo $! > %s" Sys.argv.(0) pid_file) in
      let () = server_pid := wait_for_the_server_to_start () in
      Lwt_io.flush_all ()
    end;

    stats = None;
    clean = begin fun () ->
      Lwt.return (clean ())
    end;

    config = Irmin_graphql.Client.config uri;
    store = graphql_store server.store;
  }

let suites servers =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    []
  else
    List.map (fun (s, server) ->
        s, suite server
      ) servers

let run_server () =
  let module Server = Irmin_unix.Graphql.Server.Make(Store)(struct let remote = Some (fun ?headers r ->
      let uri = Uri.of_string r in
      match Uri.scheme uri with
      | Some "file" ->
        let cfg = Irmin_git.config (Uri.path uri) in
        let store = Lwt_main.run (Disk.Repo.v cfg >>= Disk.master) in
        Irmin.remote_store (module Disk) store
      | _ -> Store.remote ?headers r)
    end) in
  let server =
    Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;
    Store.Repo.v (Irmin_mem.config ()) >>= Store.master >>= fun t ->
    server_pid := Unix.getpid ();
    Conduit_lwt_unix.set_max_active 100;
    let server = Server.server t in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 80808)) server
  in
  Lwt_main.run server

let servers = [
  `Quick, Test_mem.suite;
  `Quick, Test_git.suite;
]

let run () =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    exit 0
  else
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "server") then
    run_server ()
  else
    Irmin_test.Store.run "irmin-graphql" ~misc:[] (suites servers)