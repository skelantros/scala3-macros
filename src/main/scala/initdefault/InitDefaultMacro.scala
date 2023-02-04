package initdefault

import scala.quoted._

object InitDefaultMacro {
    private def defaultValueExpr[A : Type](using quotes: Quotes): Option[Expr[DefaultValue[A]]] = 
        Expr.summon[DefaultValue[A]]

    private def namedArgOfCaseField(using quotes: Quotes)(tpe: quotes.reflect.TypeRepr)(fieldSymbol: quotes.reflect.Symbol): quotes.reflect.NamedArg = {
        import quotes.reflect.*
        val typeRef = tpe.memberType(fieldSymbol)
        val defValueExpr = typeRef.asType match {
            case '[t] => defaultValueExpr[t].get
        }
        NamedArg(fieldSymbol.name, '{${defValueExpr}.x}.asTerm)
    }

    private def nameTypesOfParams(using quotes: Quotes)(funcSymbol: quotes.reflect.Symbol): Set[(String, quotes.reflect.Symbol)] = 
        funcSymbol.paramSymss.head.map(s => (s.name, s.termRef.typeSymbol)).toSet

    private def findCaseClassConstructor(using quotes: Quotes)(singletonSymbol: quotes.reflect.Symbol, caseFields: List[quotes.reflect.Symbol]): Option[quotes.reflect.Symbol] = {
        import quotes.reflect.*
        val caseFieldsNameType = caseFields.map(s => (s.name, s.termRef.typeSymbol)).toSet
        singletonSymbol.declaredMethod("apply").find(nameTypesOfParams(_) == caseFieldsNameType)
    }

    def initCaseClassMacro[A : Type](using quotes: Quotes): Expr[A] = {
        import quotes.reflect.*
        val tpe = TypeRepr.of[A]
        val typeSymbol = tpe.typeSymbol
        val caseFields = typeSymbol.caseFields
        val namedArgs = caseFields.map(namedArgOfCaseField(tpe))

        val singletonSymbol = typeSymbol.companionModule
        val applyFunc = singletonSymbol.declaredField("apply")

        val applyMethodSymbol = findCaseClassConstructor(singletonSymbol, caseFields).get

        val tree = Apply(Select(Ident(singletonSymbol.termRef), applyMethodSymbol), namedArgs)

        tree.asExprOf[A]
    }
}
