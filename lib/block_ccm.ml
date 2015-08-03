open Lwt

module Make(B : V1_LWT.BLOCK) = struct

  type key = {
    key :Nocrypto.Cipher_block.AES.CCM.key;
    maclen: int;
    nonce_len: int}

  type t = {
    raw : B.t;
    mutable k: key option;
    sector_len: int;
    sectors: int64;
    s: Cstruct.t;
  }

  let disconnect eb = B.disconnect eb.raw

  let s0s1 eb =
    Cstruct.sub eb.s 0 eb.sector_len,
    Cstruct.sub eb.s eb.sector_len eb.sector_len

  let kmn {key;maclen;nonce_len} = key, maclen, nonce_len

  let read_internal eb k sector buffer =
    let key,maclen,nonce_len=kmn k in
    let s = Int64.mul sector 2L in
    let s0,s1 = s0s1 eb in
    B.read eb.raw s [s0; s1] >>= function
    | `Error _ as e -> return e
    | `Ok () ->
      let c = Cstruct.sub eb.s 0 (eb.sector_len + maclen) in
      let nonce = Cstruct.sub eb.s (eb.sector_len + maclen) nonce_len in
      let adata = Cstruct.create 0 in
      match Nocrypto.Cipher_block.AES.CCM.decrypt ~key ~nonce ~adata c with
      | Some plain ->
        Cstruct.blit plain 0 buffer 0 eb.sector_len;
        return (`Ok ())
      | None -> return (`Error (`Unknown "decrypt error"))

  let write_internal eb k sector p =
    let key,maclen,nonce_len=kmn k in
    let s = Int64.mul sector 2L in
    let nonce = Nocrypto.Rng.generate nonce_len in
    let adata = Cstruct.create 0 in
    let c = Nocrypto.Cipher_block.AES.CCM.encrypt ~key ~nonce ~adata p in
    let s0,s1 = s0s1 eb in
    Cstruct.blit c 0 s0 0 eb.sector_len;
    Cstruct.blit c eb.sector_len s1 0 maclen;
    Cstruct.blit nonce 0 s1 maclen nonce_len;
    B.write eb.raw s [s0;s1]


  (** Call [fn sector page] for each page in each buffer. *)
  let each_page eb key sector_start buffers fn =
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
    match eb.k with | Some k ->
      each_page eb k sector_start buffers (fun sector page ->
          read_internal eb k sector page )
    | None -> return (`Error (`Unknown "not keyed"))

  let write eb sector_start buffers =
    match eb.k with | Some k ->
    each_page eb k sector_start buffers (fun sector page ->
        write_internal eb k sector page )
    | None -> return (`Error (`Unknown "not keyed"))

  type info = {
    read_write : bool;
    sector_size : int;
    size_sectors : int64;
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

  type id = string
  let id eb =  "foo"
  type page_aligned_buffer = B.page_aligned_buffer

  let set_key ?maclen ?nonce_len key eb =
    let maclen = match maclen with | None -> 8 | Some x -> x in
    let nonce_len = match nonce_len with | None -> 8 | Some x -> x in
    let key = Nocrypto.Cipher_block.AES.CCM.of_secret ~maclen key in
    eb.k <- Some {key; maclen; nonce_len}

  let unset_key eb = eb.k <- None

  let connect raw =
    B.get_info raw >>= fun raw_info ->
    assert(raw_info.B.sector_size = 512);
    let sectors = Int64.div raw_info.B.size_sectors 2L in
    let s = Io_page.get 1  |> Io_page.to_cstruct in
    let k = None in
    return (`Ok {raw; sector_len=raw_info.B.sector_size; sectors; k; s})
end
