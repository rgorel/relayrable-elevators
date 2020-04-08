package com.relayr.elevators

import scala.collection.immutable.Queue

package object models {
  type PickupRequests = Queue[PickupRequest]

  val EmptyPickupRequests: PickupRequests = Queue()
}
