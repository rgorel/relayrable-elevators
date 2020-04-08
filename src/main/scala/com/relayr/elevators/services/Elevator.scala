package com.relayr.elevators.services

import com.relayr.elevators.models._
import scala.util.{Failure, Success}

final case class Elevator(state: ElevatorState) {
  def schedule(floor: Int): ElevatorState.Moving = state match {
    case ElevatorState.Stopped(currentFloor) =>
      ElevatorState.Moving(currentFloor = currentFloor, targetFloor = floor)

    case movingElevator: ElevatorState.Moving => movingElevator.currentPath.add(floor) match {
      case Failure(WrongDirection) =>
        movingElevator
          .nextPath
          .map { path =>
            movingElevator.copy(nextFloor = Some(path.add(floor).get.toFloor))
          }
          .getOrElse(movingElevator.copy(nextFloor = Some(floor)))

      case Success(path) => movingElevator.copy(targetFloor = path.toFloor)
      case Failure(exception: Throwable) => throw exception
    }
  }

  def advance(): ElevatorState = state match {
    case stopped: ElevatorState.Stopped => stopped
    case moving: ElevatorState.Moving if moving.currentPath.completed =>
      moving
        .nextPath
        .map(path =>
          ElevatorState.Moving(currentFloor = path.nextFloor, targetFloor = path.toFloor).finalized
        )
        .getOrElse(ElevatorState.Stopped(moving.currentFloor))

    case moving: ElevatorState.Moving =>
      moving.copy(currentFloor = moving.currentPath.nextFloor).finalized
  }
}

object Elevator {
  def create(floor: Int) = Elevator(ElevatorState.Stopped(floor))
}
