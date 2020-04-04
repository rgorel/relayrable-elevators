package com.relayr.elevators

import com.relayr.elevators.models._

final case class Dispatcher(state: State) {
  def pickup(pickupRequest: PickupRequest): State = ???

  def step(): State = ???
}

object Dispatcher {
  def createState(numElevators: Int): State =
    State(
      elevators = 0.until(numElevators).map { _ =>
        ElevatorState(currentFloor = 1, targetFloor = 1)
      }
    )
}
