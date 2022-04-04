open Lwt.Infix

module Make(BLOCK : Mirage_block.S) = struct

  type key = { key       : Mirage_crypto.Cipher_block.AES.CCM.key;
               maclen    : int;
               nonce_len : int
             }

  type t = { raw        : BLOCK.t;
             k          : key;
             sector_len : int;
             sectors    : int64;
             s          : Cstruct.t
           }

  type error = [ Mirage_block.error | `Block of BLOCK.error | `DecryptError ]

  type write_error = [ Mirage_block.write_error | `Block of BLOCK.write_error ]

  let pp_error pp = function
    | #Mirage_block.error as e -> Mirage_block.pp_error pp e
    | `Block e -> BLOCK.pp_error pp e
    | `DecryptError -> Format.fprintf pp "decrypt error"

  let pp_write_error pp = function
    | #Mirage_block.write_error as e -> Mirage_block.pp_write_error pp e
    | `Block e -> BLOCK.pp_write_error pp e

  let disconnect eb = BLOCK.disconnect eb.raw

  let s0s1 eb =
    Cstruct.(sub eb.s 0 eb.sector_len,
             sub eb.s eb.sector_len eb.sector_len)

  let kmn { key; maclen; nonce_len } = key, maclen, nonce_len

  let sector = Int64.mul 2L

  let read_internal eb s buffer =
    let key, maclen, nonce_len = kmn eb.k in
    let s0, s1 = s0s1 eb in
    BLOCK.read eb.raw (sector s) [s0; s1] >>= function
    | Error (#Mirage_block.error as e) -> Lwt.return (Error e)
    | Error e -> Lwt.return (Error (`Block e))
    | Ok () ->
      let c, nonce, adata =
        Cstruct.(sub eb.s 0 (eb.sector_len + maclen),
                 sub eb.s (eb.sector_len + maclen) nonce_len,
                 sub eb.s (eb.sector_len + maclen + nonce_len)
                   (eb.sector_len - maclen - nonce_len)) in
      match Mirage_crypto.Cipher_block.AES.CCM.authenticate_decrypt ~key ~nonce ~adata c with
      | Some plain ->
        Cstruct.blit plain 0 buffer 0 eb.sector_len;
        Lwt.return (Ok ())
      | None -> Lwt.return (Error `DecryptError)

  let write_internal eb s p =
    let key, maclen, nonce_len = kmn eb.k in
    let fill = Mirage_crypto_rng.generate (eb.sector_len - maclen) in
    let nonce, adata =
      Cstruct.(sub fill 0 nonce_len,
               sub fill nonce_len (eb.sector_len - maclen - nonce_len)) in
    let c = Mirage_crypto.Cipher_block.AES.CCM.authenticate_encrypt ~key ~nonce ~adata p in
    let s0,s1 = s0s1 eb in
    Cstruct.(blit c 0 s0 0 eb.sector_len;
             blit c eb.sector_len s1 0 maclen;
             blit fill 0 s1 maclen (eb.sector_len - maclen));
    BLOCK.write eb.raw (sector s) [s0; s1] >|= function
    | Error (#Mirage_block.write_error as err) -> Error err
    | Error e -> Error (`Block e)
    | Ok () -> Ok ()

  (** Call [fn sector page] for each page in each buffer. *)
  let each_page eb sector_start buffers fn =
    let do_buffer sector buffer =
      let len = Cstruct.length buffer in
      let rec loop_page s i =
        if i = len then Lwt.return (Ok ())
        else (
          let page = Cstruct.sub buffer i eb.sector_len in
          fn s page >>= function
          | Error _ as e -> Lwt.return e
          | Ok () -> loop_page (Int64.add s 1L) (i + eb.sector_len)
        ) in
      loop_page sector 0 in
    let rec loop s = function
      | [] -> Lwt.return (Ok ())
      | b :: bs ->
        do_buffer s b >>= function
        | Error _ as e -> Lwt.return e
        | Ok  () ->
          loop (Int64.add s (Cstruct.length b / eb.sector_len |> Int64.of_int)) bs
    in
    loop sector_start buffers

  let read eb sector_start buffers =
    each_page eb sector_start buffers (read_internal eb)

  let write eb sector_start buffers =
    each_page eb sector_start buffers (write_internal eb)

  let get_info eb =
    BLOCK.get_info eb.raw >>= fun raw_info ->
    Lwt.return {
      Mirage_block.read_write = raw_info.Mirage_block.read_write;
      sector_size = raw_info.Mirage_block.sector_size;
      size_sectors = eb.sectors;
    }

  let connect ?(maclen = 8) ?(nonce_len = 8) ~key raw =
    BLOCK.get_info raw >>= fun raw_info ->
    let key = Mirage_crypto.Cipher_block.AES.CCM.of_secret ~maclen key in
    assert(raw_info.Mirage_block.sector_size > (( maclen + nonce_len) * 2));
    let k = {key; maclen; nonce_len} in
    let sectors = Int64.div raw_info.Mirage_block.size_sectors 2L in
    let sector_len = raw_info.Mirage_block.sector_size in
    let s = Cstruct.create (sector_len * 2) in
    Lwt.return ({ raw; sector_len; sectors; k; s })
end
