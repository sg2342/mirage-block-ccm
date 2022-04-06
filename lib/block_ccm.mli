module Make (B: Mirage_block.S) : sig

  type error = [ Mirage_block.error | `Block of B.error | `DecryptError ]

  type write_error = [ Mirage_block.write_error | `Block of B.write_error ]

  include Mirage_block.S
    with type error := error
     and type write_error := write_error

  val connect : ?maclen:int -> ?nonce_len:int -> key:Cstruct.t -> B.t -> t Lwt.t
end
