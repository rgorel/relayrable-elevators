package com.relayr.elevators.cli

import com.relayr.elevators.models.{PickupRequest, State}
import com.relayr.elevators.MaxElevators
import com.relayr.elevators.services.Dispatcher

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Controller {
  def apply(): Unit = {
    welcome()
    repl(initializeState())
  }

  def welcome(): Unit =
    println("Welcome to Relayrable Elevator Simulation!")

  def usage(): Unit =
    println(
      """
      | Here's what you can do:
      |   s
      |     request status of all elevators
      |   d [ID] [FLOOR]
      |     request elevator with ID [ID] to go to the [FLOOR]. First space can be omitted. Examples: d 0 10, d3 1
      |   p [FLOOR] [DIRECTION]
      |     pickup from [FLOOR] and go to [DIRECTION]. [DIRECTION] is either `u` (up) or `d` (down).
      |     Spaces can be omitted. Examples: p 10 u, p5d
      |   n
      |     perform the next step of the simulation
      |   q
      |     quit
      """.stripMargin
    )

  @tailrec
  def initializeState(): State = {
    println(s"How many elevators do you want to run? [1..$MaxElevators]")

    Try { StdIn.readInt() } match {
      case Success(numElevators: Int) if 1 until MaxElevators contains numElevators =>
        Dispatcher.createState(numElevators)
      case Success(_) | Failure(_: NumberFormatException) => {
        println("Wrong value, try again")
        initializeState()
      }
    }
  }

  @tailrec
  def repl(state: State): Unit = {
    usage()

    Action(StdIn.readLine("choose > ")) match {
      case Some(Action.Status) => {
        Printer(state).status()
        repl(state)
      }

      case Some(Action.Destination(elevatorId: Int, targetFloor: Int)) =>
        repl(Dispatcher(state).newDestination(elevatorId, targetFloor))

      case Some(Action.Pickup(pickupRequest: PickupRequest)) =>
        repl(Dispatcher(state).pickup(pickupRequest))

      case Some(Action.Step) => {
        val nextState = Dispatcher(state).step()
        Printer(nextState).status()
        repl(nextState)
      }

      case Some(Action.Quit) => println("Ok, all elevators destroyed! Bye bye!")

      case None => {
        println("Unable to parse input, try again")
        repl(state)
      }
    }
  }
}
