package com.relayr.elevators.cli

import com.relayr.elevators.models._

final case class Printer(state: State) {
  def status(): Unit = {
    println("Elevators: (current floor -> target floor -> next floor):")

    state.elevators.zipWithIndex.foreach { case (elevator, id) =>
      val statusLine: String = elevator match {
        case ElevatorState.Stopped(_) => "[stopped]"
        case ElevatorState.Moving(_, targetFloor, nextFloor) =>
          s"-> $targetFloor " + nextFloor.map("-> " + _).getOrElse("")
      }
      println(s"\t# $id:\t${elevator.currentFloor} $statusLine")
    }

    println("Pending pickup requests: (floor + direction)")

    val pickupRequestStatuses: Seq[String] = state.pendingPickupRequests.map(pickupRequest =>
      pickupRequest.pickupFloor.toString + directionSymbol(pickupRequest.direction)
    )

    println("\t" + (if (pickupRequestStatuses.isEmpty) "none" else pickupRequestStatuses.mkString(", ")))
  }

  private def directionSymbol(direction: Direction): String = direction match {
    case Direction.Up => "↑"
    case Direction.Down => "↓"
  }
}
