open Lwt.Infix
open Test_graphql

module Client =
  Irmin_unix.Graphql.Client.Make_ext
    (Irmin_git.Metadata)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Store.Hash)

let test_set_get _branch client =
  Client.set client ["a"; "b"; "c"] "123" >>= unwrap >>= fun _ ->
  Client.get client ["a"; "b"; "c"] >>= unwrap >>= fun s ->
  Client.set client ["foo"] "bar" >>= unwrap >>= fun _ ->
  Client.get client ["foo"] >>= unwrap >|= fun s' ->
  Alcotest.(check string) "get a/b/c" "123" s;
  Alcotest.(check string) "get foo" "bar" s'

let test_head branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  Client.commit_info client hash >>= unwrap >>= fun info' ->
  check_type_eq "Commit info" Irmin.Info.t info.info info'.info;
  Client.set client ["foo"] "baz" >>= unwrap >|= fun hash' ->
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
  Store.Tree.to_concrete tree >>= fun tree' ->
  Client.update_tree client [] tree' >>= unwrap >>= fun hash' ->
  Store.Tree.get_tree tree  ["test"] >>= fun tree ->
  check_type_not_eq "hash after update tree" Store.Hash.t hash hash';
  Client.get_tree client ["test"] >>= unwrap >|= Store.Tree.of_concrete >>= fun tree' ->
  check_type_eq "tree after update tree" Store.tree_t tree tree';
  Store.Tree.remove tree ["a"] >>= Store.Tree.to_concrete >>=
  Client.set_tree client ["test"] >>= unwrap >>= fun _ ->
  Client.get_tree client ["test"] >>= unwrap >>= fun tree' ->
  let tree' = Store.Tree.of_concrete tree' in
  Store.Tree.mem tree' ["a"] >|= fun exists ->
  Alcotest.(check bool) "test/a removed" false exists

let repo_path =
  let p = Unix.getcwd () / "_test_repo" in
  "file://" ^ p

let test_clone _branch client =
  (Client.clone client repo_path >>= function
  | Ok _ -> Lwt.return_unit
  | Error (`Msg msg) -> Alcotest.failf "clone: %s" msg)

let test_pull branch client =
  (* See https://github.com/mirage/irmin/issues/600 *)
  if branch <> "master" then Lwt.return_unit
  else
    (Client.pull client repo_path >>= function
    | Ok _ -> Lwt.return_unit
    | Error (`Msg msg) -> Alcotest.failf "pull: %s" msg) >>= fun () ->
    Client.get client ["d"] >|= function
    | Ok x -> Alcotest.(check string) "after pull" (branch ^ "4") x
    | Error (`Msg msg) -> Alcotest.failf "after pull error: %s" msg

let test_set_get_all _branch client =
  let key = ["x"] in
  let value = "testing" in
  Client.set_all client key value `Everybody >>= function
  | Ok _ ->
    (Client.get_all client key >|= function
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
      List.sort String.compare (List.split branches |> fst)
    in
    Alcotest.(check (list string)) "Branches" l l'
  | Error (`Msg msg) -> Alcotest.failf "branches: %s" msg

let test_snapshot_revert _branch client =
  Client.set client ~author:"AAA"  ~message:"BBB"  ["something"] "abc" >>= function
  | Ok hash ->
    (Client.set client ["something"] "xyz" >>= fun _ ->
     Client.revert client hash >>= fun _ ->
     Client.get client ["something"] >|= function
     | Ok x -> Alcotest.(check string) "Get after revert" "abc" x
     | Error (`Msg msg) -> Alcotest.failf "revert: %s" msg)
  | Error (`Msg msg) ->
    Alcotest.failf "set: %s" msg

let test_lca branch client =
  Client.branch_info client branch >>= function
  | Ok commit ->
    let commit = List.hd commit.parents in
    (Client.lca client commit.Client.hash >|= function
      | Ok commits ->
        Alcotest.(check bool) "LCA" true
          (List.exists (fun x -> x.Client.hash = commit.Client.hash) commits)
      | Error (`Msg msg) -> Alcotest.failf "branch_info: %s" msg)
  | Error (`Msg msg) -> Alcotest.failf "branch_info: %s" msg

let test_push _branch client =
  let remote = "file://" ^ (Unix.getcwd () / "_push") in
  Client.push client remote >|= function
  | Ok _ -> ()
  | Error (`Msg msg) -> Alcotest.failf "push: %s" msg

let tests = [
  "set/get", `Quick, test_set_get;
  "branch_info/commit_info", `Quick, test_head;
  "remove", `Quick, test_remove;
  "tree", `Quick, test_tree;
  "clone", `Quick, test_clone;
  "pull", `Quick, test_pull;
  "set_all/get_all", `Quick, test_set_get_all;
  "branches", `Quick, test_branches;
  "snapshot/revert", `Quick, test_snapshot_revert;
  "lca", `Quick, test_lca;
  "push", `Quick, test_push;
]

let run_tests name tests =
  let client = Client.v uri in
  let t branch =
    let client = Client.with_branch client (Some branch) in
    List.map (fun (name, speed, f) ->
        let f () =
          ignore (Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0));
          (*init_test_repo branch >>= fun () ->*)
          f branch client
        in
        branch ^ ":" ^ name, speed, (fun () -> Lwt_main.run (f ()))
      ) tests
  in
  let a = t "master" in
  let b = t "testing" in
  Alcotest.run name [name, a @ b]

let run () =
  run_tests "CLIENT_EXT" tests
