structure Rlp :> RLP = struct

datatype value = ByteArray of Word8Vector.vector | Sequence of value list

fun serializeInteger (n : int) : Word8Vector.vector =
    let
        fun loop (w : LargeWord.word) =
            let
                val byte = Word8.fromLarge w
                val rest = LargeWord.>> (w, 0w8)
            in
                if rest = 0w0 then
                    Word8Vector.fromList [byte]
                else
                    Word8Vector.append (loop rest, byte)
            end
    in
        loop (LargeWord.fromInt n)
    end

fun deserializeInteger (bytes : Word8Vector.vector) : int =
    LargeWord.toInt
        (Word8Vector.foldl
             (fn (x, acc) =>
                 LargeWord.<< (acc, 0w8) + Word8.toLarge x) 0w0 bytes)

local
    structure V = Word8Vector

    fun serializeByteArray (bytes : V.vector) : V.vector =
        let
            val len = V.length bytes
        in
            if len = 1 andalso V.sub (bytes, 0) < 0w128 then
                bytes
            else if len < 56 then
                V.prepend (Word8.fromInt (len + 128), bytes)
            else
                let
                    val prefix = serializeInteger (len)
                in
                    V.prepend (Word8.fromInt (V.length prefix + 183),
                               V.concat [prefix, bytes])
                end
        end

    and serializeSequence (values : value list) : V.vector =
        let
            val serialized = V.concat (List.map serializeValue values)
            val len = V.length serialized
        in
            if len < 56 then
                V.prepend (Word8.fromInt (len + 192), serialized)
            else
                let
                    val prefix = serializeInteger (len)
                in
                    V.prepend (Word8.fromInt (V.length prefix + 247),
                               V.concat [prefix, serialized])
                end
        end

    and serializeValue (value : value) : V.vector =
        case value of
            ByteArray bytes => serializeByteArray bytes
          | Sequence values => serializeSequence values
in
val serialize : value -> V.vector = serializeValue
end

local
    structure S = Word8VectorSlice

    fun deserializeSliceInteger (bytes : S.slice) : int =
        LargeWord.toInt
            (S.foldl
                 (fn (x, acc) =>
                     LargeWord.<< (acc, 0w8) + Word8.toLarge x) 0w0 bytes)

    fun deserializeSliceSequence (bytes : S.slice) : value =
        let
            fun loop (bytes : S.slice) =
                if S.length bytes = 0 then
                    []
                else
                    let
                        val (value, rest) = deserializeSliceValue bytes
                    in
                        value :: loop rest
                    end
        in
            Sequence (loop bytes)
        end

    and deserializeSliceValue (bytes : S.slice) : (value * S.slice) =
        let
            val first = Word8.toInt (S.sub (bytes, 0))
        in
            if first > 247 then
                let
                    val len = deserializeSliceInteger
                                  (S.subslice (bytes, 0, SOME (first - 247)))
                in
                    (deserializeSliceSequence
                        (S.subslice (bytes, 1 + first - 247, SOME (len))),
                     S.subslice (bytes, 1 + first - 247 + len, NONE))
                end
            else if first > 192 then
                (deserializeSliceSequence
                     (S.subslice (bytes, 1, SOME (first - 192))),
                 S.subslice (bytes, 1 + first - 192, NONE))
            else if first > 183 then
                let
                    val len = deserializeSliceInteger
                                  (S.subslice (bytes, 0, SOME (first - 183)))
                in
                    ((ByteArray o S.vector o S.subslice)
                         (bytes, 1 + first - 183, SOME (len)),
                     S.subslice (bytes, 1 + first - 183 + len, NONE))
                end
            else if first > 128 then
                ((ByteArray o S.vector o S.subslice)
                     (bytes, 1, SOME (first - 128)),
                 S.subslice (bytes, 1 + first - 128, NONE))
            else
                ((ByteArray o S.vector o S.subslice) (bytes, 0, SOME (1)),
                 S.subslice (bytes, 1, NONE))
        end

in
fun deserialize (bytes : Word8Vector.vector) : value =
    let
        val (value, rest) = deserializeSliceValue (S.full bytes)
    in
        (* TODO: assert (S.length rest = 0) *)
        value
    end
end

fun toString (value : value) : string =
    case value of
        ByteArray bytes => "<"
                           ^ String.concatWith
                                 " " (List.map Word8.toString
                                               (Word8Vector.toList bytes))
                           ^ ">"
     |  Sequence values => "["
                           ^ String.concatWith
                                 " " (List.map toString values)
                           ^ "]"
end
