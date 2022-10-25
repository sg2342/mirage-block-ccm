open Lwt.Infix

module CCM = Block_ccm.Make(Block)

let opts = function
  | (None, _, _) | (_, None, _) | (_, _, None) ->
    Lwt.fail (Failure "Key In and Out required")
  | (Some key, Some src, Some dst) ->
    let strip_0x s = match Astring.String.cut ~sep:"0x" s with
      | Some ("", x) -> x
      | _ -> s in
    try
      let key = Cstruct.of_hex (strip_0x key) in
      Lwt.return (key, src, dst)
      with | Invalid_argument _ -> Lwt.fail (Failure "invalid key")

let unix_err = function
  | Error e -> Lwt.fail_with (Fmt.to_to_string Block.pp_error e)
  | Ok () -> Lwt.return_unit

let unix_write_err = function
  | Error e -> Lwt.fail_with (Fmt.to_to_string Block.pp_write_error e)
  | Ok () -> Lwt.return_unit

let ccm_err = function
  | Error e -> Lwt.fail_with (Fmt.to_to_string CCM.pp_error e)
  | Ok () -> Lwt.return_unit

let ccm_write_err = function
  | Error e -> Lwt.fail_with (Fmt.to_to_string CCM.pp_write_error e)
  | Ok () -> Lwt.return_unit

let run t =
  try
    Lwt_main.run t;
    `Ok ()
  with
  | Failure x -> `Error(false, x)

let sector () = Cstruct.create 512

let create_dst fn sectors =
  let open Lwt_unix in
  openfile fn [ Unix.O_CREAT; Unix.O_RDWR ] 0o0644 >>= fun fd ->
  LargeFile.lseek fd Int64.(mul 512L (sub sectors 1L)) SEEK_CUR >>= fun _ ->
  let sector = sector () in
  Cstruct.memset sector 0xff;
  Block.really_write fd sector >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Block.connect fn

let disconnect ccm_dev dst_dev src_dev =
  CCM.disconnect ccm_dev >>= fun () ->
  Block.disconnect dst_dev >>= fun () ->
  Block.disconnect src_dev >>= fun () ->
  Lwt.return ()

let copy (readf, err) (writef, write_err) count =
  let sector = sector () in
  let rec aux offset = function
    | 0L -> Lwt.return ()
    | remaining ->
      readf offset [sector] >>= err >>= fun () ->
      writef offset [sector] >>= write_err >>= fun () ->
      aux Int64.(add offset 1L) Int64.(sub remaining 1L)
  in
  aux 0L count

let encrypt o =
  Mirage_crypto_rng_unix.initialize ();
  let t =
    opts o >>= fun (key, src, dst) ->
    Block.connect src >>= fun src_dev ->
    Block.get_info src_dev >>= fun src_info ->
    let sectors = src_info.size_sectors in
    create_dst dst Int64.(mul 2L sectors) >>= fun dst_dev ->
    CCM.connect ~key dst_dev >>= fun ccm_dev ->
    copy
      (Block.read src_dev, unix_err)
      (CCM.write ccm_dev, ccm_write_err)
      sectors >>= fun () ->
    disconnect ccm_dev dst_dev src_dev in
  run t

let decrypt o =
  let t =
    opts o >>= fun (key, src, dst) ->
    Block.connect src >>= fun src_dev ->
    CCM.connect ~key src_dev >>= fun ccm_dev ->
    CCM.get_info ccm_dev >>= fun ccm_info ->
    let sectors = ccm_info.size_sectors in
    create_dst dst sectors >>= fun dst_dev ->
    copy
      (CCM.read ccm_dev, ccm_err)
      (Block.write dst_dev, unix_write_err)
      sectors >>= fun () ->
    disconnect ccm_dev dst_dev src_dev in
  run t

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
    Arg.(value & opt (some string) None & info ["k"; "key"]
           ~docv:"Key" ~doc ~docs) in
  let i_s =
    let docs = copts_sect in
    let doc = "Path to source disc image file." in
    Arg.(value & opt (some string) None & info ["i"; "in"]
           ~docv:"src.img" ~doc ~docs) in
  let i_d =
    let docs = copts_sect in
    let doc = "Path to destination disc image file." in
    Arg.(value & opt (some string) None & info ["o"; "out"]
           ~docv:"dst.img" ~doc ~docs) in
  let make i_k i_s i_d = (i_k, i_s, i_d) in
  Term.(const make $ i_k $ i_s $ i_d)

let enc_cmd =
  let doc = "encrypt plain disc image" in
  let man = [
    `S "DESCRIPTION";
    `P "Create AES-CCM disk image. The destination image will twice as large as
        the source image"] in
  let term = Term.(ret (const encrypt $ common_options_t))
  and info = Cmd.info "enc" ~doc ~man
  in
  Cmd.v info term

let dec_cmd =
  let doc = "decrypt AES-CCM disc image" in
  let man = [
    `S "DESCRIPTION";
    `P "Create plain disk image. The destination image will half as large as
        the source image"] in
  let term = Term.(ret (const decrypt $ common_options_t))
  and info = Cmd.info "dec" ~doc ~man
  in
  Cmd.v info term

let cmds = [enc_cmd; dec_cmd]

let help_cmd =
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common_options_t))

let () =
  let doc = "convert plain disc images from/to AES-CCM encrypted images" in
  let man = help_secs in
  let info = Cmd.info "ccmblock" ~sdocs:copts_sect ~doc ~man in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval group)
