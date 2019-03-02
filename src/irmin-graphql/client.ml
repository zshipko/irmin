open Lwt.Infix

include Client_ext

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

      let add t x =
        Graphql.Private.add_object t (Irmin.Type.to_string C.t x) >|= Graphql.unwrap

      let merge: [`Read | `Write] t -> key option Irmin.Merge.t = fun t ->
        let f = Irmin.Merge.f C.merge in
        Irmin.(Merge.v (Type.option Key.t) (fun ~old a b ->
          let _ = old in
          (match a with
          | Some a ->
              Graphql.find_object t a >|= fun a ->
              Graphql.unwrap a
          | None -> Lwt.return_none) >>= fun a ->
          (match b with
          | Some b ->
              Graphql.find_object t b >|= fun b ->
              Graphql.unwrap b
          | None -> Lwt.return_none) >>= fun b ->
          old () >>= (function
            | Ok (Some (Some old)) ->
                Graphql.find_object t old >|= fun old ->
                Merge.promise (Graphql.unwrap old)
            | Ok None | Ok (Some None) ->
                Lwt.return (Merge.promise None)
            | Error e -> Lwt.return (fun () -> Lwt.return_error e)) >>= fun old ->
          f ~old a b >>= function
          | Ok (Some x) -> add t x >|= fun x -> Ok (Some x)
          | Ok None -> Lwt.return_ok None
          | Error e -> Lwt.return_error e
        ))

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

      let add t v =
        let v = Irmin.Type.to_string Val.t v in
        Graphql.Private.add_node t v >|= Graphql.unwrap


      let find t hash =
        (Graphql.Private.find_node t hash >|= Graphql.unwrap) >|= function
        | Some x -> Some (Irmin.Type.of_string Val.t x |> Graphql.unwrap)
        | None -> None

      let mem t hash =
        find t hash >|= function
        | Some _ -> true
        | None -> false
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

      let add t commit =
        let parents = Val.parents commit in
        let node = Val.node commit in
        Graphql.Private.add_commit t ~parents node >|= Graphql.unwrap

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
        Graphql.verify client >|= fun ok ->
        if not ok then
          failwith "Unable to verify client"
        else client

      let batch t f =
        let node = t, t in
        let commit = node, t in
        f t node commit
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

      let test_and_set t branch ~test ~set =
        let _ = test in
        match set with
        | Some set ->
          Graphql.set_branch t branch set >|= Graphql.unwrap
        | None ->
          Graphql.remove_branch t branch >|= Graphql.unwrap

      let watch t ?init x =
        let _ = t, init, x in
        Lwt.return_unit

      let watch_key t key ?init x =
        let _ = t, key, init,x in
        Lwt.return_unit

      let unwatch t x =
        let _ = t, x in
        Lwt.return_unit
    end
  end

  include Irmin.Of_private (X)

  (*let parent_hashes = function None -> None | Some l -> Some (List.map Commit.hash l)
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

  let set_tree ?retries ?allow_empty ?parents ~info t key tree =
    let parents = parent_hashes parents  in
    let author, message = split_info info in
    Tree.to_concrete tree >>= fun tree->
    Graphql.set_tree (repo t) ?retries ?allow_empty ?parents ~author ~message key tree >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> Graphql.unwrap e

  let set_tree_exn ?retries ?allow_empty ?parents ~info t key tree =
    set_tree ?retries ?allow_empty ?parents ~info t key tree >|= function
    | Ok () -> ()
    | Error _ -> invalid_arg "set_tree"

  (*let with_tree = unimplemented "with_tree"
  let with_tree_exn = unimplemented "with_tree_exn"*)

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

  let merge ?retries ?allow_empty ?parents ~info ~old t key v =
    let parents = parent_hashes parents  in
    let author, message = split_info info in
    Graphql.merge (repo t) ?retries ?allow_empty ?parents ~author ~message ~old key v >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> Graphql.unwrap e

  let merge_exn ?retries ?allow_empty ?parents ~info ~old t key v =
    merge ?retries ?allow_empty ?parents ~info ~old t key v >|= function
    | Ok () -> ()
    | Error _ -> invalid_arg "merge_exn"*)

end

module KV (Client: Cohttp_lwt.S.Client)(Contents: Irmin.Contents.S) =
  Make
    (Client)
    (Irmin.Metadata.None)
    (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
