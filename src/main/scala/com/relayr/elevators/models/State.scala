package com.relayr.elevators.models

final case class State(
  elevators: Seq[ElevatorState]
)

final case class ElevatorState(
  currentFloor: Int,
  targetFloor: Int,
  queuedPickups: Seq[PickupRequest] = Seq()
)
