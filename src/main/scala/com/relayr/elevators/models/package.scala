package com.relayr.elevators

package object models {
  import scala.collection.immutable.Queue

  type PickupRequests = Map[Int, Queue[PickupRequest]]
}

