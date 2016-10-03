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
  | `Ok x -> f x
  | `Error e -> assert_equal e `Unimplemented; return ()

let sectors () =
  let page = (Io_page.get 1  |> Io_page.to_cstruct) in
  Cstruct.(sub page 0 512, sub page 512 1024)

let write_one _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    let s0,s1 = sectors () in
    CCM.write ccm 0L [s0; s1] >>|= fun () ->
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    return () in
  Lwt_main.run t

let write_then_read _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    let s0, s1 = sectors () in
    CCM.write ccm 0L [s0; s1] >>|= fun () ->
    CCM.read ccm 0L [s1; s0] >>|= fun () ->
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    return () in
  Lwt_main.run t

let fail_read _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    let maclen, nonce_len, key  = 8, 8, key `K32 in
    CCM.connect ~nonce_len ~maclen ~key dev >>= fun ccm ->
    let s0,_ = sectors () in
    CCM.read ccm 0L [s0] >>= fun r ->
    assert_equal r (`Error (`Unknown "decrypt error"));
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    return () in
  Lwt_main.run t

let read_error _ =
  let t =
    Fake_block.connect (Some 0) >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    Fake_block.disconnect dev >>= fun () ->
    let s0, _ = sectors () in
    CCM.read ccm 0L [s0] >>|=  return in
  Lwt_main.run t

let coverage _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    CCM.get_info ccm >>= fun _ ->
    let _ = CCM.id ccm in
    return () in
  Lwt_main.run t

let suite =
  "All" >::: ["write_one" >:: write_one;
              "write_then_read" >:: write_then_read;
              "read_error" >:: read_error;
              "fail_read" >:: fail_read;
              "coverage" >:: coverage]
