module type S = sig
  type store

  val schema : store -> unit Graphql_lwt.Schema.schema
  val start_server : ?port:int -> store -> unit Lwt.t
end

module type STORE = sig
  include Irmin.S
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
  val string_of_contents: 'a Irmin.Type.t -> 'a -> string
  val contents_of_string: 'a Irmin.Type.t -> string -> ('a, [`Msg of string]) result
end

module Make(Store : STORE) : S with type store = Store.t
