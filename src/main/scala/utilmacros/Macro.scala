package utilmacros

import scala.quoted._

object Macro {
    private def printWithPrinter(using quotes: Quotes)(printer: quotes.reflect.Printer[quotes.reflect.Tree], tree: quotes.reflect.Tree): Unit = {
        import quotes.reflect.*
        println(tree.show(using printer))
    }

    private def printTree(using quotes: Quotes)(tree: quotes.reflect.Tree): Unit = 
        printWithPrinter(quotes.reflect.Printer.TreeStructure, tree)

    private def printCode(using quotes: Quotes)(tree: quotes.reflect.Tree): Unit = 
        printWithPrinter(quotes.reflect.Printer.TreeAnsiCode, tree)

    def printTreeMacro[A : Type](x: Expr[A])(using quotes: Quotes): Expr[Unit] = {
        import quotes.reflect.*
        val tree = x.asTerm
        printTree(tree)
        '{()}
    }

    def printCodeMacro[A : Type](x: Expr[A])(using quotes: Quotes): Expr[Unit] = {
        import quotes.reflect.*
        val tree = x.asTerm
        printCode(tree)
        '{()}
    }
}
