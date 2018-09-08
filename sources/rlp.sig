signature RLP = sig
    type value

    val ByteArray : Word8Vector.vector -> value
    val Sequence : value list -> value

    val serializeInteger : int -> Word8Vector.vector
    val deserializeInteger : Word8Vector.vector -> int

    val serialize : value -> Word8Vector.vector
    val deserialize : Word8Vector.vector -> value

    val toString : value -> string
end
