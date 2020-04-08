package com.relayr.elevators.services

import com.relayr.elevators.models._
import org.scalatest.funspec.AnyFunSpec

import scala.collection.immutable.Queue

class DispatcherSpec extends AnyFunSpec {
  val state = State(
    elevators = Vector(
      Elevator.create(5).schedule(3),
      ElevatorState.Stopped(10),
      Elevator.create(7).schedule(12)
    )
  )

  def getState(result: Either[ElevatorNotFound, State]): State = result match {
    case Left(exception: Exception) => throw exception
    case Right(state) => state
  }

  describe("scheduling a new destination for an elevator") {
    val updated = getState(Dispatcher(state).newDestination(0, 2))

    it("schedules the destination") {
      val elevatorState = updated.elevators(0).asInstanceOf[ElevatorState.Moving]
      assert(elevatorState.targetFloor == 2)
    }
  }

  describe("going to the next step of the simulation") {
    val updated = Dispatcher(state).step()

    it("moves the elevators") {
      val movedElevator = updated.elevators(0).asInstanceOf[ElevatorState.Moving]
      val stoppedElevator = updated.elevators(1).asInstanceOf[ElevatorState.Stopped]

      assert(movedElevator.currentFloor == 4)
      assert(stoppedElevator.currentFloor == 10)
    }
  }

  describe("scheduling pickup requests") {
    val updated: State = Seq(
      PickupRequest(8, Direction.Up),
      PickupRequest(6, Direction.Down),
      PickupRequest(1, Direction.Down)
    ).foldLeft(state)(
      (state, pickupRequest) => getState(Dispatcher(state).pickup(pickupRequest))
    )

    it("tells corresponding elevators to move towards pickup requests") {
      val elevators = updated.elevators.asInstanceOf[IndexedSeq[ElevatorState.Moving]]

      assert(elevators(0).targetFloor == 1)
      assert(elevators(0).nextFloor.isEmpty)
      assert(elevators(1).targetFloor == 6)
      assert(elevators(1).nextFloor.isEmpty)
      assert(elevators(2).targetFloor == 12)
      assert(elevators(2).nextFloor.isEmpty)
    }
  }

  describe("when all the elevators are moving in one direction") {
    val state = State(
      elevators = IndexedSeq(
        Elevator.create(10).schedule(4),
        Elevator.create(4).schedule(3),
        Elevator.create(8).schedule(6)
      )
    )

    describe("and there's a pickup request in the other direction") {
      val updated = getState(Dispatcher(state).pickup(PickupRequest(9, Direction.Up)))

      it("chooses an elevator with the closest turning point") {
        val elevators = updated.elevators.asInstanceOf[IndexedSeq[ElevatorState.Moving]]

        assert(elevators(0).targetFloor == 4)
        assert(elevators(0).nextFloor.isEmpty)
        assert(elevators(1).targetFloor == 3)
        assert(elevators(1).nextFloor.isEmpty)
        assert(elevators(2).targetFloor == 6)
        assert(elevators(2).nextFloor.contains(9))
      }
    }

    describe("and there's a pickup request in the same direction") {
      val updated = getState(Dispatcher(state).pickup(PickupRequest(5, Direction.Down)))

      it("chooses an elevator that is going to continue travelling in the same direction after pickup") {
        val elevators = updated.elevators.asInstanceOf[IndexedSeq[ElevatorState.Moving]]

        assert(elevators(0).targetFloor == 4)
        assert(elevators(0).nextFloor.isEmpty)
        assert(elevators(1).targetFloor == 3)
        assert(elevators(1).nextFloor.isEmpty)
        assert(elevators(2).targetFloor == 6)
        assert(elevators(2).nextFloor.isEmpty)
      }
    }
  }

  describe("when there are pending pickup requests on the floors with elevators") {
    val stateWithPickups = state.copy(
      pendingPickupRequests = Queue(
        PickupRequest(10, Direction.Up),
        PickupRequest(7, Direction.Up),
        PickupRequest(1, Direction.Down)
      )
    )

    it("serves pickup requests") {
      val updated = Dispatcher(stateWithPickups).step()

      assert(updated.pendingPickupRequests == Queue(PickupRequest(1, Direction.Down)))
    }
  }
}
