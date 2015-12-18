open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let pkg_config flags package =
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd

let pkg_config_lib ~lib ~has_lib ~stublib =
  let cflags = (A has_lib) :: pkg_config "cflags" lib in
  let stub_l = [A (Printf.sprintf "-l%s" stublib)] in
  let libs_l = pkg_config "libs-only-l" lib in
  let libs_L = pkg_config "libs-only-L" lib in
  let linker = match os with
    | "Linux\n" -> [A "-Wl,-no-as-needed"]
    | _ -> []
  in
  let make_opt o arg = S [ A o; arg ] in
  let mklib_flags = (List.map (make_opt "-ldopt") linker) @ libs_l @ libs_L in
  let compile_flags = List.map (make_opt "-ccopt") cflags in
  let lib_flags = List.map (make_opt "-cclib") libs_l in
  let link_flags = List.map (make_opt "-ccopt") (linker @ libs_L) in
  let stublib_flags = List.map (make_opt "-dllib") stub_l  in
  let tag = Printf.sprintf "use_%s" lib in
  flag ["c"; "ocamlmklib"; tag] (S mklib_flags);
  flag ["c"; "compile"; tag] (S compile_flags);
  flag ["link"; "ocaml"; tag] (S (link_flags @ lib_flags));
  flag ["link"; "ocaml"; "library"; "byte"; tag] (S stublib_flags)

let () =
  dispatch (function
    | After_options ->
      let ocamldoc tags deps docout docdir =
        let tags = tags -- "extension:html" in
        Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir in
        
      rule "ocamldoc with plugin"
        ~prod:"%.docdir/index.html"
        ~stamp:"%.docdir/html.stamp"
        ~dep:"%.odocl"
        (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
           ~ocamldoc "%.odocl" "%.docdir/index.html" "%.docdir");

      pflag ["ocaml"; "doc"; "docdir"] "plugin"
        (fun plugin -> S [A "-g"; A plugin]);

    | After_rules ->
      pkg_config_lib ~lib:"libudev" ~has_lib:"-DHAS_LIBUDEV" ~stublib:"udev_stubs"
    | _ -> ())
