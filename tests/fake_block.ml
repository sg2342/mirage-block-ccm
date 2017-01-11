(* Copyright (C) 2014, Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt

module A = Bigarray.Array1

type +'a io = 'a Lwt.t
type t = Cstruct.t
type error = [ Mirage_block.error | `DecryptError ]
let pp_error pp = function
  | #Mirage_block.error as err -> Mirage_block.pp_error pp err
  | `DecryptError -> Format.fprintf pp "decrypt error"
type write_error = Mirage_block.write_error
let pp_write_error = Mirage_block.pp_write_error

let sector_size = 512

let safe_of_int64 i64 =
  let i = Int64.to_int i64 in
  assert (Int64.of_int i = i64);
  i

let write device sector_start buffers =
  let rec loop dstoff = function
    | [] -> ()
    | x :: xs ->
        Cstruct.blit x 0 device dstoff (Cstruct.len x);
        loop (dstoff + (Cstruct.len x)) xs in
  loop (safe_of_int64 sector_start * sector_size) buffers;
  Ok () |> return

let read device sector_start buffers =
  if 0 = (Cstruct.len device) then return (Error `Unimplemented)
  else
    let rec loop dstoff = function
      | [] -> ()
      | x :: xs ->
        Cstruct.blit device dstoff x 0 (Cstruct.len x);
        loop (dstoff + (Cstruct.len x)) xs in
    loop (safe_of_int64 sector_start * sector_size) buffers;
    Ok () |> return

let info = {
  Mirage_block.read_write = true;
  sector_size;
  size_sectors = 64L;
}

let size = info.Mirage_block.sector_size * safe_of_int64 info.Mirage_block.size_sectors

let get_info _device = return info

let disconnect _device = return ()

(* let id _device = () *)

type page_aligned_buffer = Cstruct.t

let connect sz =
  let sz = match sz with | None -> size | Some x -> x in
  let data = Cstruct.create sz in
  if sz != 0 then Cstruct.blit_from_string (String.make sector_size (Char.chr 0)) 0 data 0 sector_size;
  return data
