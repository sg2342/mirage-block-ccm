module Make (B: V1.BLOCK
             with type 'a io = 'a Lwt.t
              and type page_aligned_buffer = Cstruct.t) : sig
  include V1.BLOCK
    with type 'a io = 'a Lwt.t
     and type page_aligned_buffer = Cstruct.t

  val connect : ?maclen:int -> ?nonce_len:int -> key:Cstruct.t -> B.t -> t io
end
