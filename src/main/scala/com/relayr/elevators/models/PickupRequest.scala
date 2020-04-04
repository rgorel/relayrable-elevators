package com.relayr.elevators.models

final case class PickupRequest(
  pickupFloor: Int,
  direction: Direction
)
