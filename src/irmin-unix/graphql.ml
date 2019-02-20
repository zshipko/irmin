module Server = struct
  module Remote = struct
    module None = struct
      let remote = None
    end
  end

  module Make(S: Irmin.S)(Remote: sig
      val remote: Resolver.Store.remote_fn option
    end) = Irmin_graphql.Server.Make(Cohttp_lwt_unix.Server)(struct
      let info = Info.v
      let remote = Remote.remote
    end)(S)
end

module Client = struct
  module Make: Irmin.S_MAKER = Irmin_graphql.Client.Make(Cohttp_lwt_unix.Client)
  module KV: Irmin.KV_MAKER = Irmin_graphql.Client.KV(Cohttp_lwt_unix.Client)
end
