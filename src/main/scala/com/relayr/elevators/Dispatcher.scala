package com.relayr.elevators

import com.relayr.elevators.models._

final case class Dispatcher(state: State) {
  def pickup(pickupRequest: PickupRequest): State =
    newDestination(findElevatorForPickup(pickupRequest), pickupRequest.pickupFloor)
      .addPickupRequest(pickupRequest)

  def newDestination(elevatorId: Int, targetFloor: Int): State = {
    val elevator: ElevatorState = state.elevators(elevatorId)

    state.copy(elevators = state.elevators.updated(elevatorId, elevator.newDestination(targetFloor)))
  }

  def step(): State =
    State(
      pendingPickupRequests = servePickupRequests(),
      elevators = moveElevators()
    )

  private def findElevatorForPickup(pickupRequest: PickupRequest): Int =
    state
      .elevators
      .zipWithIndex
      .sortBy { case(elevator, id) =>
        if (elevator.currentPath.map(_.sameDirection(pickupRequest)).getOrElse(true))
          // elevator is going in pickup direction or stopped:
          // high priority, sort by distance from current to pickup floor
          (0, FloorPath(elevator.currentFloor, pickupRequest.pickupFloor).distance)
        else
          // elevator is going in the opposite direction:
          // low priority, sort by distance from turning point to pickup floor
          (1, FloorPath(elevator.nextFloor.get, pickupRequest.pickupFloor).distance)
      }
      .head._2

  private def moveElevators(): IndexedSeq[ElevatorState] =
    state.elevators.map(_.move())

  private def servePickupRequests(): PickupRequests =
    state.pendingPickupRequests.filterNot { case (floor, _) =>
      state.elevators.exists(_.currentFloor == floor)
    }
}

object Dispatcher {
  def createState(numElevators: Int): State =
    State(
      elevators = 0.until(numElevators).map { index: Int =>
        ElevatorState(currentFloor = 1)
      }
    )
}
