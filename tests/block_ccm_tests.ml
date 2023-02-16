open OUnit2
open Lwt.Infix

module CCM = Block_ccm.Make(Fake_block)

let key k =
  let s = match k with
    | `K16 -> "00112233445566778899aabbccddeeff"
    | `K32 -> "00112233445566778899aabbccddeeff" ^
              "00112233445566778899aabbccddeeff" in
  Cstruct.of_hex s

let (>>|=) m f = m >>= function
  | Ok x -> f x
  | Error e -> assert_equal e `Disconnected; Lwt.return ()

let sectors () =
  Cstruct.(create 512, create 512)

let write_one _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    let s0,s1 = sectors () in
    CCM.write ccm 0L [s0; s1] >>|= fun () ->
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    Lwt.return () in
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
    Lwt.return () in
  Lwt_main.run t

let fail_read _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    let nonce_len, key  = 8, key `K32 in
    CCM.connect ~nonce_len ~key dev >>= fun ccm ->
    let s0,_ = sectors () in
    CCM.read ccm 0L [s0] >>= fun r ->
    assert_equal r (Error `DecryptError);
    CCM.disconnect ccm >>= fun () ->
    Fake_block.disconnect dev >>= fun () ->
    Lwt.return () in
  Lwt_main.run t

let read_error _ =
  let t =
    Fake_block.connect (Some 0) >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    Fake_block.disconnect dev >>= fun () ->
    let s0, _ = sectors () in
    CCM.read ccm 0L [s0] >>|= Lwt.return in
  Lwt_main.run t

let coverage _ =
  let t =
    Fake_block.connect None >>= fun dev ->
    CCM.connect ~key:(key `K16) dev >>= fun ccm ->
    CCM.get_info ccm >>= fun _ ->
    Lwt.return () in
  Lwt_main.run t

let suite =
  "All" >::: ["write_one" >:: write_one;
              "write_then_read" >:: write_then_read;
              "read_error" >:: read_error;
              "fail_read" >:: fail_read;
              "coverage" >:: coverage]
