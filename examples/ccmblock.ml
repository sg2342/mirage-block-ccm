
let encrypt = function
  | (None, _, _) | (_, None, _) | (_, _, None) ->
    `Error (true, "-k -s and -d required")
  | _ -> `Ok ()

let decrypt = function
  | (None, _, _) | (_, None, _) | (_, _, None) ->
    `Error (true, "-k -s and -d required")
  | _ -> `Ok ()


(** command line interface *)
open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."]

let common_options_t =
  let docs = copts_sect in
  let i_k =
    let doc = "specified as hexadecimal string." in
    Arg.(value & opt (some string) None & info ["k"] ~docv:"key" ~doc ~docs) in
  let i_s =
    let docs = copts_sect in
    let doc = "Path to source disc image file." in
    Arg.(value & opt (some string) None & info ["s"]
           ~docv:"src.img" ~doc ~docs) in
  let i_d =
    let docs = copts_sect in
    let doc = "Path to destination disc image file." in
    Arg.(value & opt (some string) None & info ["d"]
           ~docv:"dst.img" ~doc ~docs) in
  let make i_k i_s i_d = (i_k, i_s, i_d) in
  Term.(pure make $ i_k $ i_s $ i_d)

let enc_cmd =
  let doc = "encrypt plain disc image" in
  let man = [
    `S "DESCRIPTION";
    `P "Create AES-CCM disk image. The destination image will twice as large as
        the source image"] in
  Term.(ret (pure encrypt $ common_options_t)), Term.info "enc" ~doc ~man

let dec_cmd =
  let doc = "decrypt AES-CCM disc image" in
  let man = [
    `S "DESCRIPTION";
    `P "Create plain disk image. The destination image will half as large as
        the source image"] in
  Term.(ret (pure decrypt $ common_options_t)), Term.info "enc" ~doc ~man

let default_cmd =
  let doc = "convert plain disc images from/to AES-CCM encrypted images" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "ccmblock" ~sdocs:copts_sect ~doc ~man

let cmds = [enc_cmd; dec_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
