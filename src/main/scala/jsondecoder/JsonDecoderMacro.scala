package jsondecoder

import scala.quoted._

object JsonDecoderMacro {
    private def nameTypeOfSymbol(using quotes: Quotes)(symbol: quotes.reflect.Symbol): (String, quotes.reflect.Symbol) = {
        import quotes.reflect.*
        (symbol.name, symbol.termRef.typeSymbol)
    }

    private def nameTypesOfParams(using quotes: Quotes)(funcSymbol: quotes.reflect.Symbol): Set[(String, quotes.reflect.Symbol)] = 
        funcSymbol.paramSymss.head.map(nameTypeOfSymbol).toSet

    private def findCaseClassConstructorMethod(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr): Option[List[quotes.reflect.Term] => quotes.reflect.Apply] = {
        import quotes.reflect.*
        val typeSymbol = tpe.typeSymbol
        val singletonSymbol = typeSymbol.companionModule
        val caseFieldsNameTypes = typeSymbol.caseFields.map(nameTypeOfSymbol).toSet
        val applyMethodSymbolOpt = singletonSymbol.methodMember("apply").find(nameTypesOfParams(_) == caseFieldsNameTypes)

        applyMethodSymbolOpt.map { applySymb =>
            args => Apply(Select(Ident(singletonSymbol.termRef), applySymb), args)
        }
    }

    private def jsonDecoderImplicit[A : Type](using quotes: Quotes): Option[Expr[JsonDecoder[A]]] = 
        Expr.summon[JsonDecoder[A]]

    private def namedArgOpt(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr, jsonValues: Expr[Map[String, Json]])(fieldSymbol: quotes.reflect.Symbol): quotes.reflect.NamedArg = {
        import quotes.reflect.*
        // possible error
        val jsonDecoder = tpe.memberType(fieldSymbol).asType match {
            case '[t] => jsonDecoderImplicit[t].get
        }
        val nameExpr = Expr(fieldSymbol.name)
        NamedArg(fieldSymbol.name, '{${jsonDecoder}(${jsonValues}(${nameExpr})).get}.asTerm)
    }

    private def caseClassInstance[A : Type](using quotes: Quotes)(jsonValues: Expr[Map[String, Json]]): quotes.reflect.Apply = {
        import quotes.reflect.*
        val tpe = TypeRepr.of[A]
        val typeSymbol = tpe.typeSymbol
        val namedArgs = typeSymbol.caseFields.map(namedArgOpt(tpe, jsonValues))
        // possible error
        val constructorFunc = findCaseClassConstructorMethod(tpe).get
        constructorFunc(namedArgs)
    }

    def decoderMacro[A : Type](using quotes: Quotes): Expr[JsonDecoder[A]] = {
        import quotes.reflect.*

        val res = '{
            new JsonDecoder[A] {
                override def apply(src: Json): Option[A] = src match {
                    case Json.Obj(fields: _*) =>
                        val jsonValues = fields.map(f => (f.key, f.value)).toMap
                        Some(${caseClassInstance[A]('jsonValues).asExprOf[A]})
                    case _ => 
                        None
                }
            }
        }
        println(res.show)
        res
    }
}