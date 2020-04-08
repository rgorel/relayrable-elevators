package com.relayr.elevators.services

import com.relayr.elevators.models.ElevatorState
import org.scalatest.funspec.AnyFunSpec

class ElevatorSpec extends AnyFunSpec {
  def elevatorWithMultipleDestinations(startFloor: Int, nextFloors: Seq[Int]): Elevator =
    nextFloors
      .foldLeft(Elevator.create(startFloor))((elevator, floor) =>
        Elevator(elevator.schedule(floor))
      )


  describe("when there are scheduled destinations") {
    val state = Elevator.create(1).schedule(5)

    it("moves towards the destination") {
      val moved = Elevator(state).advance().asInstanceOf[ElevatorState.Moving]

      assert(state.targetFloor  == 5)
      assert(moved.targetFloor == 5)
      assert(moved.currentFloor == 2)
      assert(moved.currentPath.toFloor == 5)
      assert(moved.currentPath.direction == 1)
      assert(moved.nextPath.isEmpty)
    }
  }

  describe("when moving down") {
    val state = Elevator.create(3).schedule(1)

    it("moves down") {
      assert(Elevator(state).advance().currentFloor == 2)
    }
  }

  describe("when multiple destinations are scheduled") {
    val elevator = elevatorWithMultipleDestinations(5, Seq(10, 15, 12, 8))

    val state = elevator.state.asInstanceOf[ElevatorState.Moving]

    it("defines optimal path") {
      assert(state.targetFloor == 15)
      assert(elevator.advance().currentFloor == 6)
      assert(state.nextPath.isEmpty)
    }

    it("schedules requests in the opposite direction after the current destination") {
      val updated: Elevator = Seq(4, 11, 2, 18)
        .foldLeft(elevator)((elevator, floor) => Elevator(elevator.schedule(floor)))

      val updatedState = updated.state.asInstanceOf[ElevatorState.Moving]

      assert(updatedState.targetFloor == 18)
      assert(updated.advance().currentFloor == 6)
      assert(updatedState.nextFloor.contains(2))
      assert(updatedState.nextPath.get.fromFloor == 18)
      assert(updatedState.nextPath.get.toFloor == 2)
      assert(updatedState.nextPath.get.direction == -1)
    }
  }

  describe("reaching the destination") {
    val state = Elevator.create(8).schedule(7)

    describe("when there are no requests in the other direction") {
      it("stops") {
        val moved = Elevator(state).advance().asInstanceOf[ElevatorState.Stopped]
        assert(moved.currentFloor == 7)
      }
    }

    describe("when there are requests in the other direction") {
      val updated = Elevator(state).schedule(12)

      it("is going to move to the other direction") {
        val moved = Elevator(updated).advance().asInstanceOf[ElevatorState.Moving]
        assert(moved.currentFloor == 7)
        assert(moved.nextFloor.contains(12))
        assert(Elevator(moved).advance().currentFloor == 8)
      }
    }
  }
}
