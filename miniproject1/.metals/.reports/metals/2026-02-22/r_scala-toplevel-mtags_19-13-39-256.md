error id: file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala:[780..786) in Input.VirtualFile("file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala", "package edu.colorado.csci3155.project1

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
case class 

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
        ???
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
}")
file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/file:<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%25202026/CSCI%25203155-100%2520Principles%2520of%2520Programming%2520Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala
file://<HOME>/Library/CloudStorage/OneDrive-UCB-O365/Spring%202026/CSCI%203155-100%20Principles%20of%20Programming%20Languages/miniproject1/project1_NicoleP/src/main/scala/edu/colorado/csci3155/project1/StackMachineEmulator.scala:25: error: expected identifier; obtained object


Current stack trace:
java.base/java.lang.Thread.getStackTrace(Thread.java:2514)
scala.meta.internal.mtags.ScalaToplevelMtags.failMessage(ScalaToplevelMtags.scala:1250)
scala.meta.internal.mtags.ScalaToplevelMtags.$anonfun$reportError$1(ScalaToplevelMtags.scala:1236)
scala.meta.internal.metals.StdReporter.$anonfun$create$1(ReportContext.scala:148)
scala.util.Try$.apply(Try.scala:217)
scala.meta.internal.metals.StdReporter.create(ReportContext.scala:143)
scala.meta.pc.reports.Reporter.create(Reporter.java:10)
scala.meta.internal.mtags.ScalaToplevelMtags.reportError(ScalaToplevelMtags.scala:1233)
scala.meta.internal.mtags.ScalaToplevelMtags.newIdentifier(ScalaToplevelMtags.scala:1107)
scala.meta.internal.mtags.ScalaToplevelMtags.emitMember(ScalaToplevelMtags.scala:788)
scala.meta.internal.mtags.ScalaToplevelMtags.loop(ScalaToplevelMtags.scala:263)
scala.meta.internal.mtags.ScalaToplevelMtags.indexRoot(ScalaToplevelMtags.scala:96)
scala.meta.internal.metals.SemanticdbDefinition$.foreachWithReturnMtags(SemanticdbDefinition.scala:83)
scala.meta.internal.metals.Indexer.indexSourceFile(Indexer.scala:560)
scala.meta.internal.metals.Indexer.$anonfun$reindexWorkspaceSources$3(Indexer.scala:691)
scala.meta.internal.metals.Indexer.$anonfun$reindexWorkspaceSources$3$adapted(Indexer.scala:688)
scala.collection.IterableOnceOps.foreach(IterableOnce.scala:630)
scala.collection.IterableOnceOps.foreach$(IterableOnce.scala:628)
scala.collection.AbstractIterator.foreach(Iterator.scala:1313)
scala.meta.internal.metals.Indexer.reindexWorkspaceSources(Indexer.scala:688)
scala.meta.internal.metals.MetalsLspService.$anonfun$onChange$2(MetalsLspService.scala:936)
scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.scala:18)
scala.concurrent.Future$.$anonfun$apply$1(Future.scala:691)
scala.concurrent.impl.Promise$Transformation.run(Promise.scala:500)
java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1144)
java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:642)
java.base/java.lang.Thread.run(Thread.java:1623)

object StackMachineEmulator {
^
#### Short summary: 

expected identifier; obtained object