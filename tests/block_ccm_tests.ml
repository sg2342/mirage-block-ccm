open OUnit2
open Lwt

module CCM = Block_ccm.Make(Fake_block)

let key k =
  let s = match k with
    | `K16 -> "00112233445566778899aabbccddeeff"
    | `K32 -> "00112233445566778899aabbccddeeff" ^
              "00112233445566778899aabbccddeeff" in
  Nocrypto.Uncommon.Cs.of_hex s

let (>>|=) m f = m >>= function
  | `Error (`Unknown s) -> fail (Failure ("E:" ^ s))
  | `Error `Disconnected -> fail (Failure ("E: `Disconnected"))
  | `Error `Is_read_only -> fail (Failure ("E: `Is_read_only"))
  | `Error `Unimplemented -> fail (Failure ("E: `Unimplemented"))
  | `Ok x -> f x

let sector () =
  let page = (Io_page.get 1  |> Io_page.to_cstruct) in
  Cstruct.sub page 0 512

let write_one _ =
  let t =
    Fake_block.connect () >>|= fun dev ->
    CCM.connect ~key:(key `K16) dev >>|= fun ccm ->
    let sector = sector () in
    CCM.write ccm 0L [sector] >>|= fun () ->
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    return () in
  Lwt_main.run t

let write_then_read _ =
  let t =
    Fake_block.connect () >>|= fun dev ->
    CCM.connect ~key:(key `K16) dev >>|= fun ccm ->
    let sector = sector () in
    CCM.write ccm 0L [sector] >>|= fun () ->
    CCM.read ccm 0L [sector] >>|= fun () ->
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    return () in
  Lwt_main.run t

let fail_read _ =
  let t =
    Fake_block.connect () >>|= fun dev ->
    CCM.connect ~key:(key `K16) dev >>|= fun ccm ->
    let sector = sector () in
    CCM.read ccm 0L [sector] >>= function
    | `Error (`Unknown "decrypt error") ->
      CCM.disconnect ccm >>= fun () ->
      Fake_block.disconnect dev >>= fun () ->
      return ()
    | `Ok () -> assert_failure "unexpected succes"
    | `Error _ -> assert_failure "unexpected error" in
  Lwt_main.run t

let suite =
  "All" >::: ["write_one" >:: write_one;
              "write_then_read" >:: write_then_read;
              "fail_read" >:: fail_read]
