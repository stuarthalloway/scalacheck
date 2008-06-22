/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://code.google.com/p/scalacheck/                                   **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import Gen._
import Prop._
import Shrink._

/** See User Guide for usage examples */
trait Commands extends Prop {

  import Prop.extendedBoolean

  /** The abstract state data type. This type must be immutable.
   *  The state type that encodes the abstract state. The abstract state
   *  should model all the features we need from the real state, the system
   *  under test. We should leave out all details that aren't needed for
   *  specifying our pre- and postconditions. The state type must be called
   *  State and be immutable. */
  type State <: AnyRef

  class Binding(private val key: State) {
    def get: Any = bindings.find(_._1 eq key) match {
      case None => error("No value bound")
      case Some(x) => x
    }
  }

  /** Abstract commands are defined as subtypes of the traits Command or SetCommand.
   *  Each command must have a run method and a method that returns the new abstract
   *  state, as it should look after the command has been run.
   *  A command can also define a precondition that states how the current
   *  abstract state must look if the command should be allowed to run.
   *  Finally, we can also define a postcondition which verifies that the
   *  system under test is in a correct state after the command exectution. */
  trait Command {

    /** Used internally. */
    protected[Commands] def run_(s: State) = run(s)

    def run(s: State): Any
    def nextState(s: State): State

    /** A precondition is a function that
     *  takes the current abstract state as parameter and returns a boolean
     *  that says if the precondition is fulfilled or not. */
    var preCondition: State => Boolean = s => true

    /** A postcondition is a function that
     *  takes two parameters, s and r. s is the abstract state before
     *  the command was run, and r is the result from the command's run
     *  method. The postcondition function should return a Boolean (or
     *  a Prop instance) that says if the condition holds or not. */
    var postCondition: (State,Any) => Prop = (s,r) => proved
  }

  /** A command that binds its result for later use */
  trait SetCommand extends Command {
    /** Used internally. */
    protected[Commands] final override def run_(s: State) = {
      val r = run(s)
      bindings += ((s,r))
      r
    }

    final def nextState(s: State) = nextState(s, new Binding(s))
    def nextState(s: State, b: Binding): State
  }

  private case class Cmds(cs: List[Command], ss: List[State]) {
    override def toString = cs.map(_.toString).mkString(", ")
  }

  private val bindings = new scala.collection.mutable.ListBuffer[(State,Any)]

  private def initState() = {
    bindings.clear()
    initialState()
  }

  private def genCmds: Gen[Cmds] = {
    def sizedCmds(s: State)(sz: Int): Gen[Cmds] =
      if(sz <= 0) value(Cmds(Nil, Nil)) else for {
        c <- genCommand(s) suchThat (_.preCondition(s))
        Cmds(cs,ss) <- sizedCmds(c.nextState(s))(sz-1)
      } yield Cmds(c::cs, s::ss)

    for {
      s0 <- value(() => initialState())
      cmds <- sized(sizedCmds(s0))
    } yield cmds
  }

  private def validCmds(s: State, cs: List[Command]): Option[Cmds] =
    cs match {
      case Nil => Some(Cmds(Nil, s::Nil))
      case c::_ if !c.preCondition(s) => None
      case c::cmds => for {
        Cmds(_, ss) <- validCmds(c.nextState(s), cmds)
      } yield Cmds(cs, s::ss)
    }

  private def runCommands(cmds: Cmds): Prop = cmds match {
    case Cmds(Nil, _) => proved
    case Cmds(c::cs, s::ss) =>
      c.postCondition(s,c.run(s)) && runCommands(Cmds(cs,ss))
    case _ => error("Should not be here")
  }

  private def commandsProp: Prop = {

    def shrinkCmds(cmds: Cmds) = cmds match { case Cmds(cs,_) =>
      shrink(cs)(shrinkList).flatMap(cs => validCmds(initialState(), cs).toList)
    }

    forAllShrink(genCmds label "COMMANDS", shrinkCmds)(runCommands _)

  }

  def apply(p: Prop.Params) = commandsProp(p)

  /** initialState should reset the system under test to a well defined
   *  initial state, and return the abstract version of that state. */
  def initialState(): State

  /** The command generator. Given an abstract state, the generator
   *  should return a command that is allowed to run in that state. Note that
   *  it is still neccessary to define preconditions on the commands if there
   *  are any. The generator is just giving a hint of which commands that are
   *  suitable for a given state, the preconditions will still be checked before
   *  a command runs. Sometimes you maybe want to adjust the distribution of
   *  your command generator according to the state, or do other calculations
   *  based on the state. */
  def genCommand(s: State): Gen[Command]

}
