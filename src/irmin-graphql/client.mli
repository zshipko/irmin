type error = [`Msg of string]

module Query: sig
  (** Generates JSON dictionary containing all default queries, this is used
      by bindings written in languages other than OCaml to remain consistent with
      the default OCaml queries. *)
  val generate_json: unit -> string
end

module Json: sig
  type t = Irmin.Contents.Json_value.t

  (** Used to find nested values in a JSON object *)
  val find: t -> string list -> t option
end

val config: Uri.t -> Irmin.config

(** {Make} is used to provide a common implementation for all backends *)
module Make(Client : Cohttp_lwt.S.Client): Irmin.S_MAKER
module KV(Client: Cohttp_lwt.S.Client): Irmin.KV_MAKER

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

  (** Initialize GraphQL client context. {headers} are sent along with the HTTP request and
      if provided {ctx} will be used as the request context.  *)
  val v: ?headers:Cohttp.Header.t -> ?ctx:Client.ctx -> ?branch:string -> Uri.t -> t

  (** Creates a client with the same connection information on a new branch *)
  val with_branch: t -> string option ->  t


  val verify: t -> bool Lwt.t

  (** {execute client ~vars ~operation query} sends a request to the specified GraphQL server
      and returns the results as a string *)
  val execute :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> string Lwt.t


  (** Similar to {execute}, but returns a JSON value result *)
  val execute_json :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> (Irmin.Contents.json, error) result Lwt.t

  (** Get a list of branch names *)
  val branches : t -> ((Branch.t * commit option) list, error) result Lwt.t


  (** Get a list of all keys *)
  val list : t -> Path.t -> (Path.t list, error) result Lwt.t

  (** {set client ~author ~message key value} sends a request to set {key} to {value} to the GraphQL
      server, if successful the new commit hash is returned *)
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

  (** Similar to {set}, but metadata may also be specified *)
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

  (** Used to replace trees at once. {set_tree} will overwrite existing trees to contain only the
   * values specified by this call *)
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

  (** Used to update a tree. {update_tree} will only add/remove the values specified by this function call *)
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

  (** Remove an item from the store *)
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

  (** {merge_with_branch client ~author ~message from} sends a request to the GraphQL server to merge {from} into the branch set on the client *)
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

  (** {find client key} will return `Ok (Some _)` if there is a value associated with {key}, otherwise `Ok None`
      if there were no errors *)
  val find :
    t -> Path.t -> (Contents.t option, error) result Lwt.t

  val find_object :
    t -> Hash.t -> (Contents.t option, error) result Lwt.t

  (** {get} is similar to find, but will return an error if the key is not found *)
  val get :
    t -> Path.t -> (Contents.t, error) result Lwt.t

  (** Get the value and metadata associated with the provided key *)
  val get_all :
    t -> Path.t -> (Contents.t * Metadata.t, error) result Lwt.t

  (** Returns the tree based at the provided key in the specified branch *)
  val get_tree :
    t -> Path.t -> (tree, error) result Lwt.t

  (** {push client uri} pushes the current branch to the remote server specified by {uri} *)
  val push :
    t -> string -> (bool, error) result Lwt.t

  (** {pull client ~author ~message uri} pulls from the remote specified by {uri} and creates a new commit *)
  val pull :
    t -> ?depth:int -> ?author:string -> ?message:string -> string -> (Hash.t, error) result Lwt.t

  (** Clone an existing Git repo *)
  val clone :
    t -> string -> (Hash.t, error) result Lwt.t

  (** Revert the specified branch to the given commit hash *)
  val revert :
    t -> Hash.t -> (bool, error) result Lwt.t

  (** Returns a list of the least common ancestors between the head of the specified branch and the provided commit hash *)
  val lca:
    t -> Hash.t -> (commit list, error) result Lwt.t

  (** Get information about a specific commit *)
  val commit_info :
    t -> Hash.t -> (commit, error) result Lwt.t

  (** Get information about a specific branch *)
  val branch_info :
    t -> Branch.t -> (commit, error) result Lwt.t

  val set_branch:
    t -> Branch.t -> Hash.t -> (bool, error) result Lwt.t

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
  end
end

module Make_ext
    (Client : Cohttp_lwt.S.Client)
    (Metadata: Irmin.Metadata.S)
    (Contents: Irmin.Contents.S)
    (Path: Irmin.Path.S)
    (Branch: Irmin.Branch.S)
    (Hash: Irmin.Hash.S):
  EXT with module Client = Client
       and module Metadata = Metadata
       and module Contents = Contents
       and module Path = Path
       and module Branch = Branch
       and module Hash = Hash

