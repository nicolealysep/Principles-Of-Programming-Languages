error id: file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala:
file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -ICondSkip.
	 -ICondSkip#
	 -ICondSkip().
	 -scala/Predef.ICondSkip.
	 -scala/Predef.ICondSkip#
	 -scala/Predef.ICondSkip().
offset: 2732
uri: file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala
text:
```scala
package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.

        TODO: Implement this function.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match{
            case Const(d) =>  List(IPush(d))
            case Id(name) => List(ILoad(name))

            case Add(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IPlus())
            }
            case Sub(e1, e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(ISub())
            }
            case Mul(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IMul())
            }
            case Div(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IDiv())
            }
            case Geq(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IGeq())
            }
            case Gt(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IGt())
            }
            case Eq(e1,e2) => {
                compileToStackMachineCode(e1)++
                compileToStackMachineCode(e2)++
                List(IEq())
            }
            case Not(e) => {
                compileToStackMachineCode(e) ++ List(INot())
                }
            case Let(name, e1, e2) => {
                compileToStackMachineCode(e1) ++
                List(IStore(name)) ++
                compileToStackMachineCode(e2) ++
                List(IPop())
            }
            case IfThenElse(cond, e1, e2) => {
                val L0 = compileToStackMachineCode(cond)
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L0 ++
                List(ICondSkip(L1.length +1))++
                L1++
                List(ICondSkip(L2.length))++
                L2
            }
            case And(e1, e2) => {
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L1 ++
                List(ICon@@dSkip(L2.length +1))++
                L2 ++
                List(ISkip(1))++
                List(IPushBool(false))
            }
            case Or(e1, e2) => {
                compileToStackMachineCode(e1)++
                List(ICondSkip(L2.length +1))++
                compileToStackMachineCode(e2)++
                List(ISkip(2))++
                List(IPushBool(true))
            }
        }
    }

}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 