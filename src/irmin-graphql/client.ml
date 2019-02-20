open Lwt.Infix

include Client_ext

exception Unimplemented of string
let unimplemented name =
  Printexc.record_backtrace true;
  raise (Unimplemented name)

module Make
    (Client : Cohttp_lwt.S.Client)
    (M: Irmin.Metadata.S)
    (C: Irmin.Contents.S)
    (K: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) =
struct
  module Graphql = Make_ext(Client)(M)(C)(K)(B)(H)


  module X = struct
    module Hash = H

    module Sync = Irmin.Private.Sync.None(H)(B)

    module Contents = struct
      module Metadata = M
      module Key = Hash
      module Val = C
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t

      let merge = unimplemented "Store.Contents.merge"
      let add = unimplemented "Store.Contents.add"

      let find t k =
        Graphql.find_object t k >|= function
        | Ok (Some x) -> Some x
        | _ -> None

      let mem t k =
        find t k >|= function
        | Some _ -> true
        | None -> false
    end

    module Node = Irmin.Private.Node.Store(Contents)(K)(M)(struct
      module Key = Hash
      module Val = Irmin.Private.Node.Make(H)(K)(M)
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t
      let add = unimplemented "Store.Node.add"
      let mem = unimplemented "Store.Node.mem"
      let find = unimplemented "Store.Node.find"
    end)


    module Commit = Irmin.Private.Commit.Store(Node)(struct
      module Key = Hash
      module Val = Irmin.Private.Commit.Make(Hash)
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t

      let make_commit c =
        let info = c.Graphql.info in
        let node = c.Graphql.hash in
        let parents = List.map (fun x -> x.Graphql.hash) c.Graphql.parents in
        Val.v ~info ~node ~parents

      let add = unimplemented "Store.Commit.add"

      let find t hash =
        Graphql.commit_info t hash >|= function
        | Ok x -> Some (make_commit x)
        | _ -> None

      let mem t hash =
        find t hash >|= function
        | Some _ -> true
        | None -> false
    end)

    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)

    module Repo = struct
      type t = Graphql.t

      let contents_t (t: t): 'a Contents.t = t
      let node_t (t: t): 'a Node.t = t, t
      let commit_t (t: t): 'a Commit.t = (node_t t), t
      let branch_t t = t

      let v config =
        let uri = get_uri config in
        let headers = None in
        let ctx = None in
        let branch = None in
        let client = Graphql.v ?headers ?ctx ?branch uri in
        Lwt.return client


      let batch = unimplemented "Repo.batch"
    end

    module Branch = struct
      module Key = B
      module Val = H
      type t = Graphql.t
      type key = Key.t
      type value = Val.t
      type watch = unit

      let find t branch =
        Graphql.branch_info t branch >|= function
        | Ok x -> Some x.Graphql.hash
        | _ -> None

      let mem t branch =
        find t branch >|= function
        | Some _ -> true
        | None -> false

      let list t =
        Graphql.branches t >|= function
        | Ok l ->
            List.map (fun (k, _) -> k) l
        | Error _ -> []

      let remove t branch =
        Graphql.remove_branch t branch >>= fun x ->
        let _ = Graphql.unwrap x in
        Lwt.return_unit

      let set t branch hash =
        Graphql.set_branch t branch hash >>= fun x ->
        let _ = Graphql.unwrap x in
        Lwt.return_unit

      let test_and_set = unimplemented "Store.Branch.test_and_set"

      let watch = unimplemented "Store.Branch.watch"
      let watch_key = unimplemented "Store.Branch.watch_key"
      let unwatch = unimplemented "Store.Branch.unwatch"
    end
  end

  include Irmin.Of_private (X)

  let parent_hashes = function None -> None | Some l -> Some (List.map Commit.hash l)
  let split_info info =
    let i = info ()  in
    Irmin.Info.author i, Irmin.Info.message i

  let find_all t key =
    Graphql.get_all (repo t) key >|= function
    | Ok (c, m) -> Some (c, m)
    | Error _ -> None

  let get_all t key =
    Graphql.get_all (repo t) key >|= function
    | Ok (c, m) -> c, m
    | Error _ as e -> Graphql.unwrap e

  let find t key =
    Graphql.find (repo t) key >|= function
    | Ok (Some x) -> Some x
    | _ -> None

  let get t key =
    find t key >|= function
    | Some x -> x
    | None -> invalid_arg "get"

  let mem t key =
    find t key >|= function
    | Some _ -> true
    | None -> false

  let remove ?retries ?allow_empty ?parents ~info t key =
    let parents = parent_hashes parents  in
    let author, message = split_info info in
    Graphql.remove (repo t) ?retries ?allow_empty ?parents ~author ~message key >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> Graphql.unwrap e

  let remove_exn ?retries ?allow_empty ?parents ~info t key =
    remove ?retries ?allow_empty ?parents ~info t key >|= function
    | Ok x -> x
    | Error _ -> invalid_arg "remove_exn"

  let set ?retries ?allow_empty ?parents ~info t key value =
    let parents = parent_hashes parents  in
    let author, message = split_info info in
    Graphql.set (repo t) ?retries ?allow_empty ?parents ~author ~message key value >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> Graphql.unwrap e

  let set_exn ?retries ?allow_empty ?parents ~info t key value =
    set ?retries ?allow_empty ?parents ~info t key value >|= function
    | Ok () -> ()
    | Error _ -> invalid_arg "set_exn"

  let test_and_set ?retries ?allow_empty ?parents ~info t key ~test ~set =
    let parents = parent_hashes parents  in
    let author, message = split_info info in
    Graphql.test_and_set (repo t) ?retries ?allow_empty ?parents ~author ~message key ~test ~set >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> Graphql.unwrap e

  let test_and_set_exn ?retries ?allow_empty ?parents ~info t key ~test ~set =
    test_and_set ?retries ?allow_empty ?parents ~info t key ~test ~set >|= function
    | Ok () -> ()
    | Error _ -> invalid_arg "test_and_set_exn"

  let tree t =
    Graphql.get_tree (repo t) K.empty >|= function
    | Ok x -> Tree.of_concrete x
    | Error _ as e -> Graphql.unwrap e

  let find_tree t key =
    Graphql.get_tree (repo t) key >|= function
    | Ok x -> Some (Tree.of_concrete x)
    | _ -> None

  let get_tree t key =
    Graphql.get_tree (repo t) key >|= function
    | Ok x -> (Tree.of_concrete x)
    | Error _ as e -> Graphql.unwrap e
end

module KV (Client: Cohttp_lwt.S.Client)(Contents: Irmin.Contents.S) =
  Make
    (Client)
    (Irmin.Metadata.None)
    (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
