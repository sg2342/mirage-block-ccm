open OUnit2

let () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  run_test_tt_main Block_ccm_tests.suite
