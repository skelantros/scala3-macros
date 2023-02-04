package jsondecoder

type JsonDecoder[A] = Decoder[Json, A]

object JsonDecoder {
    import Json._

    type Primitive = String | Double | Boolean

    def apply[A](json: Json)(using dec: JsonDecoder[A]): Option[A] = dec(json)

    private inline def pfToDecoder[A](pf: PartialFunction[Json, A]): JsonDecoder[A] = new JsonDecoder[A] {
        def apply(src: Json): Option[A] = pf.lift(src)
    }

    given intDec: JsonDecoder[Int] = pfToDecoder[Int] {
        case Number(x) if x.isWhole => x.toInt
    }
    
    given doubleDec: JsonDecoder[Double] = pfToDecoder {
        case Number(x) => x
    }

    given boolDec: JsonDecoder[Boolean] = pfToDecoder {
        case Bool(x) => x
    }

    given strDec: JsonDecoder[String] = pfToDecoder {
        case Str(x) => x
    }

    given seqDec[A : JsonDecoder]: JsonDecoder[Seq[A]] = pfToDecoder {
        case Arr(jsonObjs: _*) if jsonObjs.forall(JsonDecoder[A](_).nonEmpty) =>
            jsonObjs.map(JsonDecoder[A](_)).collect {
                case Some(x) => x
            }
    }
}

inline def jsonDecoderForClass[A]: JsonDecoder[A] = ${JsonDecoderMacro.decoderMacro[A]}