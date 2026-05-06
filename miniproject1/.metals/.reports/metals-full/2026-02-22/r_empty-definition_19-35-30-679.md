error id: file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala:scala/Predef.assert(+1).
file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -assert.
	 -assert#
	 -assert().
	 -scala/Predef.assert.
	 -scala/Predef.assert#
	 -scala/Predef.assert().
offset: 4903
uri: file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala
text:
```scala
package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- TODO: Complete the inductive definition of the remaining 
          byte code instructions as specified 
          in the documentation --*/


case class ICondSkip(n: Int) extends StackMachineInstruction
case class ISkip(n: Int) extends StackMachineInstruction

case class IPush(v: Double) extends StackMachineInstruction
case class ILoad(name: String) extends StackMachineInstruction
case class IStore(name: String) extends StackMachineInstruction
case class IPlus() extends StackMachineInstruction
case class IMul() extends StackMachineInstruction
case class ISub() extends StackMachineInstruction
case class IDiv() extends StackMachineInstruction
case class IGeq() extends StackMachineInstruction
case class IGt() extends StackMachineInstruction
case class IEq() extends StackMachineInstruction
case class INot() extends StackMachineInstruction
case class IPop() extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type EnvStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
        TODO: Implement this function.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: EnvStack,
                                 ins: StackMachineInstruction): (OpStack, EnvStack) = {
        ins match{
            case IPush(d) => {
                (Num(d):: stack, env)
            }
            case IStore(name) => {
                assert(stack.nonEmpty)
                val v = stack.head
                val r = stack.tail
                (r, (name,v) :: env)
            }
            case ILoad(name) => {
                val m = env.find{
                    case(x,_) => x == name
                }
                assert(m.nonEmpty)
                val v = m.get._2
                (v:: stack, env)
            }
            case IPlus() =>{
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                val rest = stack.drop(2)
                (Num(v2+v1) :: rest, env)
            }
            case IMul() => {
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                val rest = stack.drop(2)
                (Num(v2*v1) :: rest, env)
            }
            case ISub() => {
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                val rest = stack.drop(2)
                (Num(v2-v1) :: rest, env)
            }
            case IDiv() => {
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                assert(v1 != 0.0)
                val rest = stack.drop(2)
                (Num(v2/v1) :: rest, env)
            }
            case IGeq()  => {
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                val rest = stack.drop(2)
                (Bool(v2 >= v1) :: rest,env)
            }
            case IGt()  => {
                assert(stack.length >=2)
                val v1 = stack.head.getDoubleValue
                val v2 = stack.tail.head.getDoubleValue
                val rest = stack.drop(2)
                (Bool(v2 > v1) :: rest,env)
            }
            case IEq()  => {
                assert(stack.length >=2)
                val a = stack.head 
                val b = stack.tail.head 
                val rest = stack.drop(2)
                (Bool(a==b) :: rest, env)
            }
            case INot()  => {
                assert(stack.nonEmpty)
                val a = stack.head.getBooleanValue
                val r = stack.tail 
                (Bool(!b):: rest,env)
            }
            case IPop()  => {
                asse@@rt(env.nonEmpty)
                (stack, env.tail)
            }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: EnvStack=Nil): (Value, EnvStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case ICondSkip(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case ISkip(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case null => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:EnvStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 