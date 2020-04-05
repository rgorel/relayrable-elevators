package com.relayr.elevators

import com.relayr.elevators.models._
import scala.io.StdIn
import scala.annotation.tailrec
import scala.util.{Try, Failure, Success}
import java.util.regex.Pattern

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
      |     request status of all evelators
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

      case Some(Action.Step) => repl(Dispatcher(state).step())

      case Some(Action.Quit) => println("Ok, all elevators destroyed! Bye bye!")

      case None => {
        println("Unable to parse input, try again")
        repl(state)
      }
    }
  }
}


sealed abstract class Action

private object Action {
  final case object Status extends Action
  final case class Destination(elevatorId: Int, targetFloor: Int) extends Action

  final case class Pickup(
    pickupRequest: PickupRequest
  ) extends Action

  final case object Step extends Action
  final case object Quit extends Action

  private val DestinationPattern = """^d\s*(\d+)\s+(\d+)""".r
  private val PickupPattern = """^p\s*(\d+)\s*(u|d)""".r

  def apply(input: String): Option[Action] = input match {
    case "s" => Some(Status)

    case DestinationPattern(elevatorId: String, targetFloor: String) =>
      Some(Destination(elevatorId.toInt, targetFloor.toInt))

    case PickupPattern(pickupFloor: String, direction: String) =>
      Some(
        Pickup(
          PickupRequest(
            pickupFloor = pickupFloor.toInt,
            direction = direction match {
              case "u" => Direction.Up
              case "d" => Direction.Down
            }
          )
        )
      )

    case "n" => Some(Step)
    case "q" => Some(Quit)

    case _ => None
  }
}
