module Make (B: V1.BLOCK
             with type 'a io = 'a Lwt.t
            and type page_aligned_buffer = Cstruct.t) : sig
  include V1.BLOCK
    with type 'a io = 'a Lwt.t
     and type page_aligned_buffer = Cstruct.t

  val connect : B.t -> [`Ok of t | `Error of error] io
  val set_key : ?maclen:int -> ?nonce_len:int -> Cstruct.t -> t -> unit
  val unset_key : t -> unit
end
