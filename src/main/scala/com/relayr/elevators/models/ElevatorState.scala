package com.relayr.elevators.models

sealed abstract class ElevatorState {
  val currentFloor: Int
}

object ElevatorState {
  final case class Stopped(currentFloor: Int) extends ElevatorState

  final case class Moving(
     currentFloor: Int,
     targetFloor: Int,
     nextFloor: Option[Int] = None
   ) extends ElevatorState {
    lazy val currentPath: FloorPath = FloorPath(currentFloor, targetFloor)
    lazy val nextPath: Option[FloorPath] = nextFloor.map(FloorPath(targetFloor, _))

    lazy val readyToStop: Boolean =
      currentPath.completed && nextPath.forall(_.completed)

    lazy val finalized: ElevatorState =
      if (readyToStop) Stopped(currentFloor) else this
  }
}

