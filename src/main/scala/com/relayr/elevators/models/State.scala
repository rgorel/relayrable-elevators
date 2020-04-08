package com.relayr.elevators.models

final case class State(
  elevators: IndexedSeq[ElevatorState],
  pendingPickupRequests: PickupRequests = EmptyPickupRequests
) {
  def addPickupRequest(pickupRequest: PickupRequest): State =
    copy(pendingPickupRequests = pendingPickupRequests.appended(pickupRequest))
}


