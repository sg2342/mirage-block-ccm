module Make (B: Mirage_types.BLOCK
             with type 'a io = 'a Lwt.t
              and type page_aligned_buffer = Cstruct.t) : sig
  include Mirage_types.BLOCK
    with type 'a io = 'a Lwt.t
     and type page_aligned_buffer = Cstruct.t

  val connect : ?maclen:int -> ?nonce_len:int -> key:Cstruct.t -> B.t -> t io
end
