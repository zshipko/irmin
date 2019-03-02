module Server: sig
  module Remote: sig
    module None: sig
      val remote: Resolver.Store.remote_fn option
    end
  end

  module Make(S: Irmin.S)(Remote: sig
    val remote: Resolver.Store.remote_fn option
  end):
    Irmin_graphql.Server.S
      with type store = S.t
       and type server = Cohttp_lwt_unix.Server.t
end

module Client: sig
  module Make : Irmin.S_MAKER
  module Make_ext
    (Metadata: Irmin.Metadata.S)
    (Contents: Irmin.Contents.S)
    (Path: Irmin.Path.S)
    (Branch: Irmin.Branch.S)
    (Hash: Irmin.Hash.S): Irmin_graphql.Client.EXT
      with module Metadata = Metadata
       and module Contents = Contents
       and module Path = Path
       and module Branch = Branch
       and module Hash = Hash
  module KV: Irmin.KV_MAKER
end
