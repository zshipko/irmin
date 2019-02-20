(** The GraphQL schema type *)
module Schema : Graphql_intf.Schema with type 'a Io.t = 'a Lwt.t

(** The type of irmin-graphql servers *)
module type S = sig
  module IO: Cohttp_lwt.S.IO
  type store
  type server

  type response_action =
    [ `Expert of Cohttp.Response.t
                 * (IO.ic
                    -> IO.oc
                    -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  val schema : store -> unit Schema.schema
  val execute_request :
      unit Schema.schema ->
      Cohttp_lwt.Request.t ->
      Cohttp_lwt.Body.t -> response_action Lwt.t
  val server : store -> server
end

(** Configuration for GraphQL servers *)
module type CONFIG = sig
  (** Determines how remotes are constructed from URLs. Set to {None} to disable interaction with remote stores *)
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  (** Determines how to build {Irmin.Info.t} values *)
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

(** {Make} is a functor that takes a {Cohttp_lwt.S.Server} implementation as well as some configuration
    information to construct an {S}. The {Config} argument can be used to disable interaction with remote stores
    by setting {remote} to {None}. *)
module Make(Server: Cohttp_lwt.S.Server)(Config: CONFIG)(Store : Irmin.S): S
  with type store = Store.t
   and type server = Server.t
