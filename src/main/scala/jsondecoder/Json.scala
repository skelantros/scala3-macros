package jsondecoder

enum Json {
    case Field(key: String, value: Json)
    case Arr(jsonObjs: (Number | Bool | Str)*)
    case Obj(jsonFields: Field*)
    case Number(x: Double)
    case Bool(x: Boolean)
    case Str(x: String)
}

object Json {
    extension (x: String) {
        def -->(y: Json): Json.Field = Field(x, y)
    }

    extension (json: Json) {
        def asString: String = Json.jsonToString(json)
    }

    given Conversion[Double, Json.Number] = Json.Number(_)
    given Conversion[Boolean, Json.Bool] = Json.Bool(_)
    given Conversion[String, Json.Str] = Json.Str(_)
    given Conversion[(String, Json), Field] = (s, j) => Field(s, j)
    given Conversion[Seq[Number | Bool | Str], Json.Arr] = js => Json.Arr(js: _*)
    given Conversion[Seq[Field], Json.Obj] = fs => Json.Obj(fs: _*)

    def jsonToString(json: Json): String = json match {
        case Number(x) => x.toString
        case Bool(x) => x.toString
        case Str(x) => s"\"$x\""
        case Field(key, value) => s"\"$key\": ${jsonToString(value)}"
        case Arr(jsonObjs @ _*) => s"[${jsonObjs.map(jsonToString).mkString(",")}]"
        case Obj(jsonFields @ _*) => s"{${jsonFields.map(jsonToString).mkString(",")}}"
    }
}