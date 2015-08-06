open OUnit2

let () =
  Nocrypto_entropy_unix.initialize ()

let () =
  run_test_tt_main Block_ccm_tests.suite
