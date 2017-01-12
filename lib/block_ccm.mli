module Make (B: Mirage_block_lwt.S) : sig

  type error = private [> Mirage_device.error | `DecryptError ]

  type write_error = private [> Mirage_device.error | `Is_read_only]

  include Mirage_block_lwt.S
    with type error := error
     and type write_error := write_error

  val connect : ?maclen:int -> ?nonce_len:int -> key:Cstruct.t -> B.t -> t Lwt.t
end
