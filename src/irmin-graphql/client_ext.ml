open Lwt.Infix

module Query = Query
module Json = Json

type error = [`Msg of string]

exception Graphql of string

(* ~uri *)
let uri: Uri.t option Irmin.Private.Conf.key =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Private.Conf.(some uri) None

let config u =
  let cfg = Irmin.Private.Conf.empty in
  Irmin.Private.Conf.add cfg uri (Some u)

let err_no_uri () = invalid_arg "Irmin_graphql client: No URI specified"

let get_uri config =
  match Irmin.Private.Conf.get config uri with
  | None   -> err_no_uri ()
  | Some u -> u

module type EXT = sig
  type t

  module Hash: Irmin.Hash.S
  module Contents: Irmin.Contents.S
  module Path: Irmin.Path.S
  module Branch: Irmin.Branch.S
  module Metadata: Irmin.Metadata.S
  module Client : Cohttp_lwt.S.Client

  type tree = [
    | `Tree of (Path.step * tree) list
    | `Contents of Contents.t * Metadata.t
  ]

  val tree_t : tree Irmin.Type.t

  type commit = {
    hash: Hash.t;
    info: Irmin.Info.t;
    parents: commit list;
    tree: tree;
  }

  val commit_t : commit Irmin.Type.t

  val v: ?headers:Cohttp.Header.t -> ?ctx:Client.ctx -> ?branch:string -> Uri.t -> t
  val with_branch: t -> string option -> t

  val verify : t -> bool Lwt.t

  val execute :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> string Lwt.t

  val execute_json :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> (Irmin.Contents.json, error) result Lwt.t

  val branches : t -> ((Branch.t * commit option) list, error) result Lwt.t

  val list : t -> Path.t -> (Path.t list, error) result Lwt.t

  val set :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> Contents.t
    -> (Hash.t, error) result Lwt.t

  val test_and_set :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> test:Contents.t option
    -> set:Contents.t option
    -> (Hash.t, error) result Lwt.t

  val set_all :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> Contents.t
    -> Metadata.t
    -> (Hash.t, error) result Lwt.t

  val set_tree :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> tree
    -> (Hash.t, error) result Lwt.t

  val update_tree :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> tree
    -> (Hash.t, error) result Lwt.t

  val remove :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> (Hash.t, error) result Lwt.t

  val merge :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> old:Contents.t option
    -> Contents.t option
    -> (Hash.t, error) result Lwt.t


  val merge_tree :
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> Path.t
    -> old:tree option
    -> tree option
    -> (Hash.t, error) result Lwt.t

  val merge_with_branch :
    t
    -> ?depth:int
    -> ?n:int
    -> ?author:string
    -> ?message:string
    -> Branch.t
    -> (Hash.t, error) result Lwt.t

  val merge_with_commit :
    t
    -> ?depth:int
    -> ?n:int
    -> ?author:string
    -> ?message:string
    -> Hash.t
    -> (Hash.t, error) result Lwt.t

  val find :
    t -> Path.t -> (Contents.t option, error) result Lwt.t

  val find_object :
    t -> Hash.t -> (Contents.t option, error) result Lwt.t

  val get :
    t -> Path.t -> (Contents.t, error) result Lwt.t

  val get_all :
    t -> Path.t -> (Contents.t * Metadata.t, error) result Lwt.t

  val get_tree :
    t -> Path.t -> (tree, error) result Lwt.t

  val push :
    t -> string -> (bool, error) result Lwt.t

  val pull :
    t -> ?depth:int -> ?author:string -> ?message:string -> string -> (Hash.t, error) result Lwt.t

  val clone :
    t -> string -> (Hash.t, error) result Lwt.t

  val revert :
    t -> Hash.t -> (bool, error) result Lwt.t

  val lca:
    t -> Hash.t -> (commit list, error) result Lwt.t

  val commit_info :
    t -> Hash.t -> (commit, error) result Lwt.t

  val branch_info :
    t -> Branch.t -> (commit, error) result Lwt.t

  val set_branch:
    t -> Branch.t -> Hash.t -> (unit, error) result Lwt.t

  val remove_branch:
    t -> Branch.t -> (bool, error) result Lwt.t

  module Private: sig

    val add_commit :
      t
      -> ?parents:Hash.t list
      -> ?author:string
      -> ?message:string
      -> Hash.t
      -> (Hash.t, error) result Lwt.t

    val add_node :
      t
      -> string
      -> (Hash.t, error) result Lwt.t

    val add_object :
      t
      -> string
      -> (Hash.t, error) result Lwt.t

    val find_node :
      t
      -> Hash.t
      -> (string option, error) result Lwt.t

    val merge_objects:
      t
      -> old:Hash.t option option
      -> Hash.t option
      -> Hash.t option
      -> (Hash.t option, error) result Lwt.t

    val test_and_set_branch:
      t
      -> test:Hash.t option
      -> set:Hash.t option
      -> (bool, error) result Lwt.t
  end
end

let opt f = function
  | None ->
    `Null
  | Some s ->
    f s

let opt_string x = opt (fun s -> `String s) x

module Helper(Client: Cohttp_lwt.S.Client)(Hash: Irmin.Hash.S) = struct
  module Client = Client

  type t = {
    uri: Uri.t;
    headers: Cohttp.Header.t option;
    ctx: Client.ctx option;
    branch: string option;
  }

  let unwrap = function
    | Ok x -> x
    | Error (`Msg msg) -> raise (Graphql msg)

  let unwrap_option name = function
    | Ok (Some x) -> Ok x
    | Ok None -> Error (`Msg ("unwrap_option: " ^ name))
    | Error e -> Error e

  let opt_branch x =
    let b = match x with
      | Some b -> b
      | None -> "master"
    in `String b

  let error msg = Error (`Msg msg)
  let error_msg msg = Error msg
  let invalid_response name = print_endline ("INVALID RESPONSE: " ^ name); error ("invalid response: " ^ name)

  let v ?headers ?ctx ?branch uri = {uri; headers; ctx; branch}
  let with_branch t branch = {t with branch = branch}

  let execute client ?vars ?operation body =
    let query = [("query", `String body)] in
    let query =
      match operation with
      | Some o ->
        ("operationName", `String o) :: query
      | None ->
        query
    in
    let query =
      match vars with
      | Some v ->
        ("variables", `O v) :: query
      | None ->
        query
    in
    let body = Cohttp_lwt.Body.of_string @@ Json.to_string (`O query) in
    Client.post ?ctx:client.ctx ?headers:client.headers ~body client.uri >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body

  let execute_json client ?vars ?operation body =
    execute client ?vars ?operation body >|= fun res ->
    Printf.printf "RESPONSE: %s\n" res;
    match Json.of_string res with
    | Ok j ->
      (match Json.find j ["errors"] with
       | Some (`A (h::_)) ->
         let s = Json.find_exn h ["message"] |> Json.get_string_exn in
         Error (`Msg s)
       | _ -> Ok j)
    | Error msg -> error_msg msg

  let decode_hash name ?(suffix = ["hash"]) key = function
    | Ok `Null -> Lwt.return_ok None
    | Ok j ->
      (match Json.find j ("data" :: key @ suffix) with
       | Some (`String hash) ->
           (match Irmin.Type.of_string Hash.t hash with
           | Ok x -> Lwt.return_ok (Some x)
           | Error e -> Lwt.return_error e)
       | _ -> Lwt.return @@ invalid_response name)
    | Error msg -> Lwt.return @@ error_msg msg

end

module Make_ext
    (Client : Cohttp_lwt.S.Client)
    (Metadata: Irmin.Metadata.S)
    (Contents: Irmin.Contents.S)
    (Path: Irmin.Path.S)
    (Branch: Irmin.Branch.S)
    (Hash: Irmin.Hash.S) =
struct

  module Metadata = Metadata
  module Contents = Contents
  module Path = Path
  module Branch = Branch
  module Hash = Hash

  include Helper(Client)(Hash)

  module Store = Irmin_mem.Make(Metadata)(Contents)(Path)(Branch)(Hash)

  type 'a transaction =
    t
    -> ?retries:int
    -> ?allow_empty:bool
    -> ?parents:Hash.t list
    -> ?author:string
    -> ?message:string
    -> 'a

  let mk_info ?author ?message ?retries ?allow_empty ?parents () =
    `O [
      ("author", opt_string author);
      ("message", opt_string message);
      ("retries", opt (fun i -> `Float (float_of_int i)) retries);
      ("allow_empty", opt (fun x -> `Bool x) allow_empty);
      ("parents", opt (fun x -> `A (List.map (fun y ->
        `String (Irmin.Type.to_string Hash.t y)) x)) parents
      );
    ]

  type tree = [
    | `Tree of (Path.step * tree) list
    | `Contents of Contents.t * Metadata.t
  ]

  let tree_t =
    let open Irmin.Type in
    mu (fun tree ->
      variant "tree" (fun tree contents -> function
        | `Tree x -> tree x
        | `Contents x -> contents x)
      |~ case1 "Tree" (list (pair Path.step_t tree)) (fun x -> `Tree x)
      |~ case1 "Contents" (pair Contents.t Metadata.t) (fun x -> `Contents x)
      |> sealv
    )

  type commit = {
    hash: Hash.t;
    info: Irmin.Info.t;
    parents: commit list;
    tree: tree;
  }

  let commit_t =
    let open Irmin.Type in
    mu (fun commit ->
      record "commit" (fun hash info parents tree -> {hash; info; parents; tree})
      |+ field "hash" Hash.t (fun t -> t.hash)
      |+ field "info" Irmin.Info.t (fun t -> t.info)
      |+ field "parents" (list commit) (fun t -> t.parents)
      |+ field "tree" tree_t (fun t -> t.tree)
      |> sealr
    )

  let verify client =
    let hash = Hash.digest "irmin" |> Irmin.Type.to_string Hash.t in
    let vars = ["hash", `String hash] in
    execute_json ~vars client "query Verify($hash: String!){ verify(hash: $hash) }" >|= function
    | Ok j ->
        (match Json.find j ["data"; "verify"] with
        | Some (`Bool true) -> true
        | _ -> false)
    | _ -> false

  let list client path =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t path in
    let vars = ["key", `String key; "branch", branch] in
    execute_json client Query.list ~vars
    >|= function
    | Ok j ->
      (match Json.find j ["data"; "branch"; "list"] with
       | Some (`A keys) ->
         (try
            Ok (List.map (function
                | `String s ->
                  (match Irmin.Type.of_string Path.t s with
                   | Error _ -> failwith "invalid key"
                   | Ok x -> x)
                | _ -> failwith "invalid key value") keys)
          with Failure msg -> Error (`Msg msg))
       | _ -> invalid_response "list")
    | Error msg -> error_msg msg

  let find client key =
    let branch = opt_branch client.branch in
    let s = Irmin.Type.to_string Path.t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute_json client ~vars Query.get >|= function
    | Ok j ->
      (match Json.find j ["data"; "branch"; "get"] with
       | Some (`String s) ->
         (match Irmin.Type.of_string Contents.t s with
          | Ok x -> Ok (Some x)
          | Error e -> Error e)
       | Some `Null -> Ok None
       | _ -> invalid_response "find")
    | Error msg -> error_msg msg

  let find_object client hash =
    let s = Irmin.Type.to_string Hash.t hash in
    let vars = [("hash", `String s)] in
    execute_json client ~vars Query.find_object >|= function
    | Ok j ->
      (match Json.find j ["data"; "find_object"] with
       | Some (`String s) ->
         (match Irmin.Type.of_string Contents.t s with
          | Ok x -> Ok (Some x)
          | Error e -> Error e)
       | Some `Null -> Ok None
       | _ -> invalid_response "find_object")
    | Error msg -> error_msg msg

  let get client key =
    find client key >|= function
    | Ok (Some x) -> Ok x
    | Ok None -> Error (`Msg ("not found: " ^ Irmin.Type.to_string Path.t key))
    | Error e -> Error e

  let get_all client key =
    let branch = opt_branch client.branch in
    let s = Irmin.Type.to_string Path.t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute_json client ~vars Query.get_all >|= function
    | Ok j ->
      (try
         let j = Json.find_exn j ["data"; "branch"; "get_all"] in
         let value =
           Json.find_exn j ["value"]
           |> Json.get_string_exn
           |> Irmin.Type.of_string Contents.t
           |> unwrap
         in
         let metadata =
           Json.find_exn j ["metadata"]
           |> Json.get_string_exn
           |> Irmin.Type.of_string Metadata.t
           |> unwrap
         in
         Ok (value, metadata)
       with
        | Failure msg -> error msg
        | Graphql msg -> error msg)
    | Error msg -> error_msg msg

  let make_tree arr =
    let tree = Store.Tree.empty in
     Lwt_list.fold_left_s (fun tree obj ->
         let key = Json.find_exn obj ["key"] |> Json.get_string_exn |> Irmin.Type.of_string Path.t |> unwrap in
         let metadata = Json.find_exn obj ["metadata"] |> Json.get_string_exn |> Irmin.Type.of_string Metadata.t |> unwrap in
         let value = Json.find_exn obj ["value"] |> Json.get_string_exn |> Irmin.Type.of_string Contents.t |> unwrap in
         Store.Tree.add tree key ~metadata value) tree arr >>= fun l ->
     Store.Tree.to_concrete l

  let get_tree client key =
    let branch = opt_branch client.branch in
    let s = Irmin.Type.to_string Path.t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute_json client ~vars Query.get_tree >>= function
    | Ok j ->
    (match Json.find j ["data"; "branch"; "get_tree"] with
     | Some (`A arr) -> make_tree arr >>= Lwt.return_ok
     | _ -> Lwt.return (invalid_response "get_tree"))
    | Error msg -> Lwt.return (error_msg msg)

  let set client ?retries ?allow_empty ?parents ?author ?message  key value =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let value = Irmin.Type.to_string Contents.t value in
    let vars =
      [ ("key", `String key)
      ; ("value", `String value)
      ; ("branch", branch)
      ; ("info", mk_info ?author ?message ?retries ?allow_empty ?parents ()) ]
    in
    execute_json client ~vars Query.set
    >>= decode_hash "set" ["set"] >|= unwrap_option "set"

  let test_and_set client ?retries ?allow_empty ?parents ?author ?message  key ~test ~set =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let test = match test with Some value -> `String (Irmin.Type.to_string Contents.t value) | None -> `Null in
    let set = match set with Some value -> `String (Irmin.Type.to_string Contents.t value) | None -> `Null in
    let vars =
      [ ("key", `String key)
      ; ("test", test)
      ; ("set", set)
      ; ("branch", branch)
      ; ("info", mk_info ?author ?message ?retries ?allow_empty ?parents ()) ]
    in
    execute_json client ~vars Query.test_and_set
    >>= decode_hash "test_and_set" ["test_and_set"] >|= unwrap_option "test_and_set"

  let set_all client ?retries ?allow_empty ?parents ?author ?message  key value metadata =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let value = Irmin.Type.to_string Contents.t value in
    let metadata = Irmin.Type.to_string Metadata.t metadata in
    let vars =
      [ ("key", `String key)
      ; ("value", `String value)
      ; ("metadata", `String metadata)
      ; ("branch", branch)
      ; ("info", mk_info ?author ?message ?retries ?allow_empty ?parents ()) ]
    in
    execute_json client ~vars Query.set_all
    >>= decode_hash "set_all" ["set_all"] >|= unwrap_option "set_all"

  let tree_list key x =
    let rec aux key x acc = match x with
      | `Contents (c, m) ->
        let k = Irmin.Type.to_string Path.t key in
        let c = Irmin.Type.to_string Contents.t c in
        let m = Irmin.Type.to_string Metadata.t m in
        `O ["key", `String k; "value", `String c; "metadata", `String m] :: acc
      | `Tree l ->
        let l = List.fold_right (fun (step, i) acc ->
            let key = Path.rcons key step in
            aux key i acc) l []
        in l
    in `A (aux key x [])

  let set_or_update_tree client ?retries ?allow_empty ?parents ?author ?message  key tree query =
    let branch = opt_branch client.branch in
    let key' = Irmin.Type.to_string Path.t key in
    let arr = tree_list key tree in
    let vars =
      [ "key", `String key'
      ; "branch", branch
      ; "info", mk_info ?author ?message ?retries ?allow_empty ?parents ()
      ; "tree", arr ]
    in
    execute_json client ~vars query

  let set_tree client ?retries ?allow_empty ?parents ?author ?message  key tree =
    set_or_update_tree client ?author ?message ?retries ?allow_empty ?parents key tree Query.set_tree
    >>= decode_hash "set_tree" ["set_tree"]  >|= unwrap_option "set_tree"

  let update_tree client ?retries ?allow_empty ?parents ?author ?message  key tree =
    set_or_update_tree client ?author ?message ?retries ?allow_empty ?parents key tree Query.update_tree
    >>= decode_hash "update_tree" ["update_tree"]  >|= unwrap_option "update_tree"

  let remove client ?retries ?allow_empty ?parents ?author ?message  key =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let vars =
      [ "key", `String key
      ; "branch", branch
      ; "info", mk_info ?author ?message ?retries ?allow_empty ?parents () ]
    in
    execute_json client ~vars Query.remove >>= decode_hash "remove" ["remove"] >|= unwrap_option "remove"

  let merge client ?retries ?allow_empty ?parents ?author ?message key ~old value =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let old = opt (fun x -> `String (Irmin.Type.to_string Contents.t x)) old in
    let value = opt (fun x -> `String (Irmin.Type.to_string Contents.t x)) value in
    let vars =
      [ "branch", branch
      ; "key", `String key
      ; "old", old
      ; "value", value
      ; "info", mk_info ?author ?message ?retries ?allow_empty ?parents () ]
    in
    execute_json client ~vars Query.merge >>= decode_hash "merge" ~suffix:[] ["merge"]
    >|= unwrap_option "merge"

  let merge_tree client ?retries ?allow_empty ?parents ?author ?message key ~old value =
    let branch = opt_branch client.branch in
    let key = Irmin.Type.to_string Path.t key in
    let old = opt (fun tree -> tree_list Path.empty tree) old in
    let value = opt (fun tree -> tree_list Path.empty tree) value in
    let vars =
      [ "branch", branch
      ; "key", `String key
      ; "old", old
      ; "value", value
      ; "info", mk_info ?author ?message ?retries ?allow_empty ?parents () ]
    in
    execute_json client ~vars Query.merge_tree >>= decode_hash "merge_tree" ["merge_tree"]
    >|= unwrap_option "merge_tree"

  let merge_with_branch client ?depth ?n ?author ?message  from =
    let into = opt_branch client.branch in
    let from = opt_branch (Some (Irmin.Type.to_string Branch.t from)) in
    let vars =
      [ "branch", into
      ; "from", from
      ; "info", mk_info ?author ?message ()
      ; "depth", opt (fun i -> `Float (float_of_int i)) depth
      ; "n", opt (fun i -> `Float (float_of_int i)) n ]
    in
    execute_json client ~vars Query.merge >>= decode_hash "merge_with_branch" ["merge_with_branch"]
    >|= unwrap_option "merge_with_branch"

  let merge_with_commit client ?depth ?n ?author ?message from =
    let into = opt_branch client.branch in
    let from = `String (Irmin.Type.to_string Hash.t from) in
    let vars =
      [ "branch", into
      ; "from", from
      ; "info", mk_info ?author ?message ()
      ; "depth", opt (fun i -> `Float (float_of_int i)) depth
      ; "n", opt (fun i -> `Float (float_of_int i)) n ]
    in
    execute_json client ~vars Query.merge_with_commit >>= decode_hash "merge_with_commit" ["merge_with_commit"]
    >|= unwrap_option "merge_with_commit"

  let push client remote =
    let branch = opt_branch client.branch in
    let vars =
      [ "branch", branch
      ; "remote", `String remote ]
    in
    execute_json client ~vars Query.push >|= function
    | Ok j ->
      (match Json.find j ["data"; "push"] with
       | Some (`Bool b) -> Ok b
       | _ -> invalid_response "push")
    | Error msg -> error_msg msg

  let pull client ?depth ?author ?message remote =
    let branch = opt_branch client.branch in
    let vars =
      [ "branch", branch
      ; "info", mk_info ?author ?message ()
      ; "remote", `String remote
      ; "depth", opt (fun x -> `Float (float_of_int x)) depth]
    in
    execute_json client ~vars Query.pull >>= decode_hash "pull" ["pull"] >|= unwrap_option "pull"

  let clone client remote =
    let branch = opt_branch client.branch in
    let vars =
      [ "branch", branch
      ; "remote", `String remote ]
    in
    execute_json client ~vars Query.clone >>= decode_hash "clone" ["clone"] >|= unwrap_option "clone"

  let revert client commit =
    let branch = opt_branch client.branch in
    let commit' = Irmin.Type.to_string Hash.t commit in
    let vars =
      [ "branch", branch
      ; "commit", `String commit' ]
    in
    execute_json client ~vars Query.revert >>= fun x ->
    (decode_hash "revert" ["revert"] x >>= function
     | Ok (Some hash) ->
       Lwt.return_ok (Irmin.Type.equal Hash.t commit hash)
     | Ok None -> invalid_arg "revert"
     | Error msg -> Lwt.return @@ error_msg msg)

  let rec make_commit_info commit =
    let hash =
      Json.find_exn commit ["hash"]
      |> Json.get_string_exn
      |> Irmin.Type.of_string Hash.t
      |> unwrap
    in
    let date =
      Json.find_exn commit ["info"; "date"]
      |> Json.get_string_exn
      |> Int64.of_string
    in
    let message =
      Json.find_exn commit ["info"; "message"]
      |> Json.get_string_exn
    in
    let author =
      Json.find_exn commit ["info"; "author"]
      |> Json.get_string_exn
    in
    let parents =
      match Json.find_exn commit ["parents"] with
      | `A a -> Lwt_list.map_s (fun x ->
          make_commit_info x) a
      | _ -> failwith "Invalid parents field"
    in
    let tree =
      match Json.find commit ["tree"] with
      | Some (`A arr) -> make_tree arr
      | Some _ -> failwith "Invalid commit field"
      | None -> make_tree []
    in
    let info = Irmin.Info.v ~date ~author message in
    parents >>= fun parents ->
    tree >|= fun tree ->
    {hash; info; parents; tree}

  let branches client =
    execute_json client Query.branches
    >>= function
    | Ok j ->
      (match Json.find_exn j ["data"; "branches"]  with
      | `A branches ->
        Lwt_list.map_p (fun obj ->
          let name = Json.find_exn obj ["name"] |> Json.get_string_exn in
          let name = Irmin.Type.of_string Branch.t name |> unwrap in
          Lwt.catch (fun () ->
            make_commit_info (Json.find_exn obj ["head"]) >>= Lwt.return_some)
          (fun _ -> Lwt.return_none) >|= fun commit -> name, commit
        ) branches >>= Lwt.return_ok
       | _ -> Lwt.return (invalid_response "branches"))
    | Error msg -> Lwt.return @@ error_msg msg

  let lca client commit =
    let branch = opt_branch client.branch in
    let commit = Irmin.Type.to_string Hash.t commit in
    let vars =
      [ "branch", branch
      ; "hash", `String commit ]
    in
    execute_json client ~vars Query.lca >>= function
    | Ok j ->
      (match Json.find j ["data"; "branch"; "lca"] with
       | Some (`A commits) ->
         Lwt.catch (fun () ->
            Lwt_list.map_p make_commit_info commits >>= Lwt.return_ok)
          (function
            | Failure msg -> Lwt.return @@ error msg
            | Invalid_argument name -> Lwt.return @@ (Error (`Msg ("invalid_argument: " ^ name)))
            | exn -> raise exn)
       | _ -> Lwt.return (invalid_response "lca"))
    | Error msg -> Lwt.return @@ error_msg msg

  let branch_info client branch =
    let branch =
      if Irmin.Type.equal Branch.t branch Branch.master
      then `Null
      else `String (Irmin.Type.to_string Branch.t branch)
    in
    let vars =
      [ "branch", branch ]
    in
    execute_json client ~vars Query.branch_info >>= function
    | Ok j ->
      (match Json.find j ["data"; "branch"; "head"] with
       | Some commit ->
         Lwt.catch (fun () ->
          make_commit_info commit >>= Lwt.return_ok)
          (function
            | Failure msg -> Lwt.return @@ error msg
            | Invalid_argument name -> Lwt.return @@ (Error (`Msg ("invalid_argument: " ^ name)))
            | exn -> raise exn)
       | None -> Lwt.return (invalid_response "branch_info"))
    | Error msg ->Lwt.return @@ error_msg msg


  let commit_info client hash =
    let commit = Irmin.Type.to_string Hash.t hash in
    let vars =
      [ "hash", `String commit ]
    in
    execute_json client ~vars Query.commit_info >>= function
    | Ok j ->
      (match Json.find j ["data"; "commit"] with
       | Some commit ->
         Lwt.catch (fun () ->
           make_commit_info commit >>= Lwt.return_ok)
          (function
            | Failure msg -> Lwt.return @@ error msg
            | Invalid_argument name -> Lwt.return @@ (Error (`Msg ("invalid_argument: " ^ name)))
            | exn -> raise exn)
       | None -> Lwt.return (invalid_response "commit_info"))
    | Error msg -> Lwt.return @@ error_msg msg

  let set_branch client branch hash =
    let branch = Irmin.Type.to_string Branch.t branch in
    let hash = Irmin.Type.to_string Hash.t hash in
    let vars =
      [ "branch", `String branch; "commit", `String hash]
    in
    execute_json client ~vars Query.set_branch >|= function
    | Ok j ->
      (match Json.find j ["data"; "set_branch"] with
       | Some (`Bool true) -> Ok ()
       | _ -> (invalid_response "set_branch"))
    | Error msg -> error_msg msg

  let remove_branch client branch  =
    let branch = Irmin.Type.to_string Branch.t branch in
    let vars =
      [ "branch", `String branch]
    in
    execute_json client ~vars Query.remove_branch >|= function
    | Ok j ->
      (match Json.find j ["data"; "remove_branch"] with
       | Some (`Bool b) -> Ok b
       | _ -> (invalid_response "remove_branch"))
    | Error msg -> error_msg msg

  module Private = struct
    let add_commit client ?parents ?author ?message node =
      let vars =
        [ "info", mk_info ?author ?message ?parents ()
        ; "node", `String (Irmin.Type.to_string Hash.t node) ]
      in
      execute_json client ~vars Query.add_commit
      >>= decode_hash "add_commit" ["add_commit"] >|= unwrap_option "add_commit"

    let add_node client node =
      let vars =
        [ "node", `String node]
      in
      execute_json client ~vars Query.add_node >|= function
      | Ok j ->
        (match Json.find j ["data"; "add_node"] with
         | Some (`String s) -> Irmin.Type.of_string Hash.t s
         | _ -> (invalid_response "add_node"))
      | Error msg -> error_msg msg

    let add_object client o =
      let vars =
        [ "object", `String o]
      in
      execute_json client ~vars Query.add_object >|= function
      | Ok j ->
        (match Json.find j ["data"; "add_object"] with
         | Some (`String s) -> Irmin.Type.of_string Hash.t s
         | _ -> (invalid_response "add_object"))
      | Error msg -> error_msg msg

    let find_node client hash =
      let vars =
        ["hash", `String (Irmin.Type.to_string Hash.t hash)]
      in
      execute_json client ~vars Query.find_node >|= function
      | Ok j ->
        (match Json.find j ["data"; "find_node"] with
         | Some (`String s) -> Ok (Some s)
         | Some `Null -> Ok None
         | _ -> (invalid_response "find_node"))
      | Error msg -> error_msg msg

    let merge_objects client ~old a b =
      let opt = function None -> `Null | Some x -> `String (Irmin.Type.to_string Hash.t x) in
      let a = opt a in
      let b = opt b in
      let old = match old with Some old -> opt old | None -> `Null in
      let vars = [
        "a", a;
        "b", b;
        "old", old;
      ] in
      execute_json client ~vars Query.merge_objects >|= function
      | Ok j ->
        (match Json.find j ["data"; "merge_objects"] with
         | Some (`String s) ->
             (match Irmin.Type.of_string Hash.t s with
             | Ok x -> Ok (Some x)
             | Error e -> Error e)
         | Some `Null -> Ok None
         | _ -> (invalid_response "merge_objects"))
      | Error msg -> error_msg msg

    let test_and_set_branch client ~test ~set =
      let opt = function None -> `Null | Some x -> `String (Irmin.Type.to_string Hash.t x) in
      let branch = opt_branch client.branch in
      let vars = [
        "test", opt test;
        "set", opt set;
        "branch", branch;
      ] in
      execute_json client ~vars Query.test_and_set_branch >|= function
      | Ok j ->
          (match Json.find j ["data"; "test_and_set_branch"] with
          | Some (`Bool b) -> Ok b
          | _ -> invalid_response "test_and_set_branch")
      | Error e -> Error e
  end
end
