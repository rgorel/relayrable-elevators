package com.relayr.elevators.models

import scala.util.{Success, Failure}
import scala.collection.immutable.Queue


final case class State(
  elevators: IndexedSeq[ElevatorState],
  pendingPickupRequests: PickupRequests = Map()
) {
  def addPickupRequest(pickupRequest: PickupRequest): State =
    copy(
      pendingPickupRequests = pendingPickupRequests + (
        pickupRequest.pickupFloor -> pendingPickupRequests
          .get(pickupRequest.pickupFloor)
          .map(_.appended(pickupRequest))
          .getOrElse(Queue(pickupRequest))
      )
    )
}

final case class ElevatorState(
  currentFloor: Int,
  targetFloor: Option[Int] = None,
  nextFloor: Option[Int] = None,
) {
  lazy val currentPath: Option[FloorPath] = targetFloor.map(FloorPath(currentFloor, _))
  lazy val nextPath: Option[FloorPath] =
    for {
      from <- targetFloor
      to <- nextFloor
    } yield FloorPath(from, to)

  def newDestination(floor: Int): ElevatorState = currentPath match {
    case None => copy(targetFloor = Some(floor))

    case (Some(pathValue)) => pathValue.add(floor) match {
      case Success(newPath: FloorPath) => copy(targetFloor = Some(newPath.toFloor))

      case Failure(WrongDirection) => nextPath match {
        case Some(nextPathValue: FloorPath) => copy(nextFloor = Some(nextPathValue.add(floor).get.toFloor))
        case None => copy(nextFloor = Some(floor))
      }

      case Failure(exception: Throwable) => throw exception
    }
  }

  def move(): ElevatorState = (currentPath, nextPath) match {
    case(None, None) => this

    case(Some(pathValue), None) if pathValue.completed =>
      copy(targetFloor = None)

    case(Some(pathValue), Some(nextPathValue)) if pathValue.completed =>
      ElevatorState(
        currentFloor = nextPathValue.nextFloor,
        targetFloor = Some(nextPathValue.toFloor),
        nextFloor = None
      )

    case(Some(pathValue), _) => copy(targetFloor = Some(pathValue.nextFloor))
  }
}
