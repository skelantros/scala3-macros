package utilmacros

inline def printObjectTree[A](inline x: A): Unit = ${Macro.printTreeMacro('x)}

inline def printObjectCode[A](inline x: A): Unit = ${Macro.printCodeMacro('x)}

inline def printObjectInfo[A](inline x: A): Unit = {
    println("tree structure:")
    printObjectTree(x)
    println("code structure:")
    printObjectCode(x)
}