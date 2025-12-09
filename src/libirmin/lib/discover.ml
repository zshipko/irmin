
 module C = Configurator.V1

module Platform = struct
  type t = Linux | Macos | Windows

  (* OS detection logic based on Revery's:
     https://github.com/revery-ui/revery/blob/master/src/Native/config/discover.re *)
  let detect_header =
    {|
#if __APPLE__
  #define PLATFORM_NAME "mac"
#elif __linux__
  #define PLATFORM_NAME "linux"
#elif WIN32
  #define PLATFORM_NAME "windows"
#endif
|}

  let detect c =
    let header =
      let file = Filename.temp_file "discover" "os.h" in
      let fd = open_out file in
      output_string fd detect_header;
      close_out fd;
      file
    in
    let header_basename = Filename.basename header in
    let header_path = Filename.dirname header in
    let c_flags = [ "-I"; header_path ] in
    let includes = [ header_basename ] in
    let platform =
      C.C_define.import c ~c_flags ~includes [ ("PLATFORM_NAME", String) ]
    in
    match platform with
    | [ (_, String "linux") ] -> Linux
    | [ (_, String "mac") ] -> Macos
    | [ (_, String "windows") ] -> Windows
    | _ -> failwith "Unsupported platform or operating system"
end

let () =
  let flags =
    [
    ]
  in
  C.main ~name:"libirmin" (fun c ->
      let platform_flags =
        match Platform.detect c with
        | Linux -> ["-ccopt"; "-Wl,-znow"]
        | Macos -> []
        | Windows -> []
      in
      C.Flags.write_sexp "link_flags.sexp" (flags @ platform_flags))
