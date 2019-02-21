let get = {|
  query Get($branch: BranchName!, $key: Key!) {
    branch(name: $branch) {
      get(key: $key)
    }
  }
|}

let get_tree = {|
  query GetTree($branch: BranchName!, $key: Key!) {
    branch(name: $branch) {
      get_tree(key: $key) {
        key
        value
        metadata
      }
    }
  }
|}

let set = {|
  mutation Set($branch: BranchName!, $key: Key!, $value: Value!, $info: InfoInput) {
    set(branch: $branch, key: $key, value: $value, info: $info) {
      hash
    }
  }
|}

let test_and_set = {|
  mutation TestAndSet($branch: BranchName!, $key: Key!, $test: Value!, $set: Value!, $info: InfoInput) {
    test_and_set(branch: $branch, key: $key, test: $test, set: $set, info: $info) {
      hash
    }
  }
|}

let update_tree = {|
  mutation UpdateTree($branch: BranchName!, $key: Key!, $tree: [TreeItem]!, $info: InfoInput) {
    update_tree(branch: $branch, key: $key, tree: $tree, info: $info) {
      hash
    }
  }
|}

let set_tree = {|
  mutation SetTree($branch: BranchName!, $key: Key!, $tree: [TreeItem!]! , $info: InfoInput) {
    set_tree(branch: $branch, key: $key, tree: $tree, info: $info) {
      hash
    }
  }
|}

let remove = {|
  mutation Remove($branch: BranchName!, $key: Key!, $info: InfoInput) {
    remove(branch: $branch, key: $key, info: $info) {
      hash
    }
  }
|}

let merge = {|
  mutation Merge($branch: BranchName, $key: Key!, $value: Value, $old: Value, $info: InfoInput) {
      merge(branch: $branch, info: $info, key: $key, value: $value, old: $old)
  }
|}

let merge_tree = {|
  mutation MergeTree($branch: BranchName, $key: Key!, $value: [TreeItem], $old: [TreeItem], $info: InfoInput) {
      merge_tree(branch: $branch, info: $info, key: $key, value: $value, old: $old) {
          hash
      }
  }
|}

let merge_with_branch = {|
  mutation MergeWithBranch($branch: BranchName, $from: BranchName!, $info: InfoInput, $max_depth: Int, $n: Int) {
      merge_with_branch(branch: $branch, from: $from, info: $info, max_depth: $max_depth, n: $n) {
          hash
      }
  }
|}

let merge_with_commit = {|
  mutation MergeWithCommit($branch: BranchName, $from: CommitHash!, $info: InfoInput, $max_depth: Int, $n: Int) {
      merge_with_commit(branch: $branch, from: $from, info: $info, max_depth: $max_depth, n: $n) {
          hash
      }
  }
|}

let push = {|
  mutation Push($branch: BranchName, $remote: Remote!) {
    push(branch: $branch, remote: $remote)
  }
|}

let pull = {|
  mutation Pull($branch: BranchName, $remote: Remote!, $info: InfoInput, $depth: Int) {
    pull(branch: $branch, remote: $remote, info: $info, depth: $depth) {
      hash
    }
  }
|}

let clone = {|
  mutation Clone($branch: BranchName, $remote: Remote!) {
    clone(branch: $branch, remote: $remote) {
      hash
    }
  }
|}

let revert = {|
  mutation Revert($branch: BranchName, $commit: CommitHash!) {
    revert(branch: $branch, commit: $commit) {
      hash
    }
  }
|}

let lca = {|
  query($branch: BranchName!, $hash: CommitHash!) {
    branch(name: $branch) {
      lca(commit: $hash) {
        hash,
        info {
          message,
          author,
          date
        }
        parents {
          hash
        }
      }
    }
  }
|}

let branch_info = {|
  query BranchInfo($branch: BranchName!) {
      branch(name: $branch) {
        name,
        head {
          hash,
          info {
            message,
            author,
            date
          }
          parents {
            hash
          }
        }
      }
  }
|}

let commit_info = {|
  query CommitInfo($hash: CommitHash!) {
    commit(hash: $hash) {
      hash,
      info {
          message,
          author,
          date
      }
      parents {
          hash
      }
    }
  }
|}

let branches = {|
  query {
    branches {
      name,
      head {
        hash,
        info {
          message,
          author,
          date
        }
        parents {
          hash
        }
      }
    }
  }
|}

let list = {|
  query List($branch: BranchName!, $key: Key) {
    branch(name: $branch) {
      list(key: $key)
    }
  }
|}

let set_branch = "mutation SetBranch($branch: BranchName!, $commit: CommitHash!) { set_branch(branch: $branch, commit: $commit) }"
let remove_branch = "mutation RemoveBranch($branch: BranchName!) { remove_branch(branch: $branch) }"

let get_all = {|
  query GetAll($branch: BranchName!, $key: Key!) {
      branch(name: $branch) {
          find_all(key: $key) {
              value
              metadata
          }
      }
  }
|}


let set_all = {|
  mutation SetAll($branch: BranchName, $key: Key!, $value: Value!, $metadata: Metadata, $info: InfoInput) {
      set_all(branch: $branch, key: $key, value: $value, metadata: $metadata, info: $info) {
          hash
      }
  }
|}


let find_object = {|
  query FindObject($hash: ObjectHash!) {
    find_object(hash: $hash)
  }
|}

let find_tree = {|
  query FindTree($hash: ObjectHash!) {
    find_tree(hash: $hash)
  }
|}


let all = [
  "get", get;
  "get_tree", get_tree;
  "set", set;
  "update_tree", update_tree;
  "set_tree", set_tree;
  "remove", remove;
  "merge", merge;
  "merge_tree", merge_tree;
  "merge_with_branch", merge_with_branch;
  "merge_with_commit", merge_with_commit;
  "push", push;
  "pull", pull;
  "clone", clone;
  "revert", revert;
  "lca", lca;
  "branch_info", branch_info;
  "commit_info", commit_info;
  "branches", branches;
  "get_all", get_all;
  "set_all", set_all;
  "list", list;
  "set_branch", set_branch;
  "remove_branch", remove_branch;
  "find_object", find_object;
  "find_tree", find_tree;
]

let generate_json () =
  let obj = List.map (fun (k, v) ->
      k, `String v) all
  in Json.to_string (`O obj)

