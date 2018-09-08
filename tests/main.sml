structure Tests = struct

val bytesToString : Word8Vector.vector -> string = Rlp.toString o Rlp.ByteArray

fun printResult prefix x = print (prefix ^ ": " ^ x ^ "\n")

fun main _ =
    let
        val () = (printResult "integer"
                  o Int.toString
                  o Rlp.deserializeInteger
                  o Rlp.serializeInteger) 10000

        val value1 = Rlp.ByteArray (Word8Vector.fromList [0wx10, 0wx20])

        val () = (printResult "serialized value1"
                  o bytesToString
                  o Rlp.serialize) value1

        val value2 = Rlp.ByteArray (Word8Vector.fromList [0wx30, 0wx40])

        val value3 = Rlp.Sequence [value1, value2]

        val () = (printResult "serialized value3"
                  o bytesToString
                  o Rlp.serialize) value3

        val () = (printResult "value3"
                  o Rlp.toString
                  o Rlp.deserialize
                  o Rlp.serialize) value3

        val value4 = Rlp.Sequence
                         [value1, Rlp.Sequence [value2, value3], value3]

        val () = (printResult "serialized value4"
                  o bytesToString
                  o Rlp.serialize) value4

        val () = (printResult "value4"
                  o Rlp.toString
                  o Rlp.deserialize
                  o Rlp.serialize) value4

        val value5 = (Rlp.ByteArray
                      o Byte.stringToBytes
                      o String.concat
                      o List.tabulate) (100, fn _ => "a")

        val () = (printResult "serialized value5"
                  o bytesToString
                  o Rlp.serialize) value5
    in
        0
    end

end
