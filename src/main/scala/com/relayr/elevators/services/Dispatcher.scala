package com.relayr.elevators.services

import com.relayr.elevators.StartFloor
import com.relayr.elevators.models._

final case class ElevatorNotFound(id: Int) extends Exception(s"Elevator #$id does not exist")

final case class Dispatcher(state: State) {
  def pickup(pickupRequest: PickupRequest): Either[ElevatorNotFound, State] =
    newDestination(findElevatorForPickup(pickupRequest), pickupRequest.pickupFloor)
      .map(_.addPickupRequest(pickupRequest))

  def newDestination(elevatorId: Int, targetFloor: Int): Either[ElevatorNotFound, State] =
    if (state.elevators.isDefinedAt(elevatorId)) {
      val elevator = Elevator(state.elevators(elevatorId))

      Right(
        state.copy(elevators = state.elevators.updated(elevatorId, elevator.schedule(targetFloor)))
      )
    }
    else
      Left(ElevatorNotFound(elevatorId))

  def step(): State =
    State(
      pendingPickupRequests = servePickupRequests(),
      elevators = moveElevators()
    )

  private def findElevatorForPickup(pickupRequest: PickupRequest): Int =
    state
      .elevators
      .zipWithIndex
      .minBy { case (elevatorState, _) =>
        pickupSortCriteria(elevatorState, pickupRequest)
      }
      ._2

  private def pickupSortCriteria(elevatorState: ElevatorState, pickupRequest: PickupRequest): (Int, Int) = {
    val distanceFromCurrentFloor = FloorPath(elevatorState.currentFloor, pickupRequest.pickupFloor).distance

    elevatorState match {
      // elevator is going in pickup direction or stopped:
      // high priority, sort by distance from current to pickup floor
      case ElevatorState.Stopped(_) =>
        (0, distanceFromCurrentFloor)

      // elevator is going in pickup direction:
      // give more priority to the one that goes further in the direction of the pickup after pickup
      case moving: ElevatorState.Moving if moving.currentPath.sameDirection(pickupRequest) =>
        (if (moving.currentPath.covers(pickupRequest.pickupFloor)) 0 else 1, distanceFromCurrentFloor)

      // elevator is going in the opposite direction:
      // low priority, sort by distance from turning point to pickup floor
      case moving: ElevatorState.Moving =>
        (2, FloorPath(moving.targetFloor, pickupRequest.pickupFloor).distance)
    }
  }

  private def moveElevators(): IndexedSeq[ElevatorState] =
    state.elevators.map(Elevator(_).advance())

  private def servePickupRequests(): PickupRequests =
    state.pendingPickupRequests.filterNot { pickupRequest =>
      state.elevators.exists(_.currentFloor == pickupRequest.pickupFloor)
    }
}

object Dispatcher {
  def createState(numElevators: Int): State =
    State(
      elevators = 0.until(numElevators).map(_ => ElevatorState.Stopped(StartFloor))
    )
}
