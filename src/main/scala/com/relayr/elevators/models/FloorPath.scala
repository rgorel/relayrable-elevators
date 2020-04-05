package com.relayr.elevators.models

import scala.util.{Try, Failure, Success}

final object WrongDirection extends Exception("impossible to reach this floor by going in this direction")

final case class FloorPath(fromFloor: Int, toFloor: Int) {
  def directionTo(floor: Int): Int = (floor - fromFloor).sign

  lazy val direction: Int = directionTo(toFloor)

  lazy val completed: Boolean = fromFloor == toFloor

  def sameDirection(floor: Int): Boolean = direction == directionTo(floor)
  def sameDirection(otherDirection: Direction): Boolean = direction == otherDirection.sign

  def sameDirection(pickupRequest: PickupRequest): Boolean =
    sameDirection(pickupRequest.pickupFloor) && sameDirection(pickupRequest.direction)

  def covers(floor: Int): Boolean = range.contains(floor)

  def add(floor: Int): Try[FloorPath] =
    if (!sameDirection(floor)) Failure(WrongDirection)
    else if (covers(floor)) Success(this)
    else Success(copy(toFloor = floor))

  lazy val distance: Int = (toFloor - fromFloor).abs

  lazy val nextFloor: Int = fromFloor + direction

  private lazy val range: Range = fromFloor to toFloor by direction
}
