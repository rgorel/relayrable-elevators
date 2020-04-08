package com.relayr.elevators.cli

import com.relayr.elevators.models._

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
