open Lwt

module Make(B : V1_LWT.BLOCK) = struct

  type key = { key       : Nocrypto.Cipher_block.AES.CCM.key;
               maclen    : int;
               nonce_len : int
             }

  type t = { raw        : B.t;
             k          : key;
             sector_len : int;
             sectors    : int64;
             s          : Cstruct.t
           }

  let disconnect eb = B.disconnect eb.raw

  let s0s1 eb =
    Cstruct.(sub eb.s 0 eb.sector_len,
             sub eb.s eb.sector_len eb.sector_len)

  let kmn { key; maclen; nonce_len } = key, maclen, nonce_len

  let sector = Int64.mul 2L

  let read_internal eb s buffer =
    let key, maclen, nonce_len = kmn eb.k in
    let s0, s1 = s0s1 eb in
    B.read eb.raw (sector s) [s0; s1] >>= function
    | `Error _ as e -> return e
    | `Ok () ->
      let c, nonce, adata =
        Cstruct.(sub eb.s 0 (eb.sector_len + maclen),
                 sub eb.s (eb.sector_len + maclen) nonce_len,
                 sub eb.s (eb.sector_len + maclen + nonce_len) 0) in
      match Nocrypto.Cipher_block.AES.CCM.decrypt ~key ~nonce ~adata c with
      | Some plain ->
        Cstruct.blit plain 0 buffer 0 eb.sector_len;
        return (`Ok ())
      | None -> return (`Error (`Unknown "decrypt error"))

  let write_internal eb s p =
    let key, maclen, nonce_len=kmn eb.k in
    let nonce = Nocrypto.Rng.generate nonce_len in
    let adata = Cstruct.create 0 in
    let c = Nocrypto.Cipher_block.AES.CCM.encrypt ~key ~nonce ~adata p in
    let s0,s1 = s0s1 eb in
    Cstruct.(blit c 0 s0 0 eb.sector_len;
             blit c eb.sector_len s1 0 maclen;
             blit nonce 0 s1 maclen nonce_len);
    B.write eb.raw (sector s) [s0;s1]


  (** Call [fn sector page] for each page in each buffer. *)
  let each_page eb sector_start buffers fn =
    let do_buffer sector buffer =
      let len = Cstruct.len buffer in
      let rec loop_page s i =
        if i = len then return (`OK ())
        else (
          let page = Cstruct.sub buffer i eb.sector_len in
          fn s page >>= function
          | `Error _ as e -> return e
          | `Ok () -> loop_page (Int64.add s 1L) (i + eb.sector_len)
        ) in
      loop_page sector 0 in
    let rec loop s = function
      | [] -> return (`Ok ())
      | b :: bs ->
        do_buffer s b >>= function
        | `Error _ as e -> return e
        | `OK  () ->
          loop (Int64.add s (Cstruct.len b / eb.sector_len |> Int64.of_int)) bs
    in
    loop sector_start buffers

  let read eb sector_start buffers =
    each_page eb sector_start buffers (fun sector page ->
        read_internal eb sector page )

  let write eb sector_start buffers =
    each_page eb sector_start buffers (fun sector page ->
        write_internal eb sector page )

  type info = {read_write   : bool;
               sector_size  : int;
               size_sectors : int64
              }

  let get_info eb =
    B.get_info eb.raw >>= fun raw_info ->
    return {
      read_write = raw_info.B.read_write;
      sector_size = raw_info.B.sector_size;
      size_sectors = eb.sectors;
    }

  type 'a io = 'a B.io
  type error = B.error
  type id = (B.t * string)

  let id eb = (eb.raw, "aes-ccm")

  type page_aligned_buffer = B.page_aligned_buffer

  let connect ?maclen ?nonce_len ~key raw =
    B.get_info raw >>= fun raw_info ->
    assert(raw_info.B.sector_size = 512);
    let maclen = match maclen with | None -> 8 | Some x -> x in
    let nonce_len = match nonce_len with | None -> 8 | Some x -> x in
    let key = Nocrypto.Cipher_block.AES.CCM.of_secret ~maclen key in
    let k = {key; maclen; nonce_len} in
    let sectors = Int64.div raw_info.B.size_sectors 2L in
    let s = Io_page.get 1  |> Io_page.to_cstruct in
    let sector_len = raw_info.B.sector_size in
    return (`Ok { raw; sector_len; sectors; k; s })
end
