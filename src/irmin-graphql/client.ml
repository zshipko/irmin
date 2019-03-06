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
        Irmin.(Merge.v (Type.option Key.t) (fun ~old a b ->
          old () >>= function
          | Ok old ->
            let old = match old with
              | Some (Some x) -> Some x
              | Some None -> None
              | None -> None
            in
            (Graphql.Private.merge_objects t ~old:(Some old) a b >|= function
            | Ok x -> Ok x
            | Error (`Msg s) ->
                Error (Graphql.unwrap (Irmin.Type.of_string Merge.conflict_t s)))
          | Error e -> Lwt.return_error e))

      let find t k =
        Graphql.find_object t k >|= function
        | Ok (Some x) -> Some x
        | Ok None -> None
        | Error _ as e -> Graphql.unwrap e

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
        let info = Val.info commit in
        let author = Irmin.Info.author info in
        let message = Irmin.Info.message info in
        Graphql.Private.add_commit t ~parents ~author ~message node >|= Graphql.unwrap

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
      module Watch = Irmin.Private.Watch.Make(Key)(Val)
      type t = Graphql.t
      type key = Key.t
      type value = Val.t
      type watch = Watch.watch


      let w = Watch.v ()

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
        Watch.notify w branch None >>= fun () ->
        Lwt.return_unit

      let set t branch hash =
        Graphql.set_branch t branch hash >|= Graphql.unwrap >>= fun () ->
        Watch.notify w branch (Some hash) >>= fun () ->
        Lwt.return_unit

      let test_and_set t branch ~test ~set =
        let branch = Irmin.Type.to_string B.t branch in
        let t = Graphql.with_branch t (Some branch) in
        Graphql.Private.test_and_set_branch t ~test ~set >|= Graphql.unwrap

      let watch _t ?init x =
        Watch.watch w ?init x

      let watch_key _t key ?init x =
        Watch.watch_key w key ?init x

      let unwatch _t x =
        Watch.unwatch w x
    end
  end

  include Irmin.Of_private (X)
end

module KV (Client: Cohttp_lwt.S.Client)(Contents: Irmin.Contents.S) =
  Make
    (Client)
    (Irmin.Metadata.None)
    (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
