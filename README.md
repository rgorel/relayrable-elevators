# Relayrable Elevator Simulation

Simulates multiple elevators serving pickup requests and scheduling multiple destination floors.

## Algorithm

Elevator moving logic:
* If elevator is stopped and is receiving a request to go to some floor, it starts moving in the direction of that floor.
* If an elevator is moving, it will continue moving in the same direction as long as there are scheduled floors for this elevator in that direction.
* When an elevator reaches its destination:
  * if there are other scheduled floors in the other direction, it starts moving in the other direction and follows the logic from the previous bullet point;
  * otherwise, the elevator stops.

Pickup request scheduling logic:
When pickup request is scheduled, we choose the elevator to serve this pickup request by the following logic (in order of decreasing priority):
* If there are stopped elevators, or elevators moving in the direction of the pickup request that will continue going in the same direction after pickup, choose the one with the shortest distance from elevator's floor to pickup floor.
* If there are elevators moving in the direction of the pickup request that won't go in the same direction after pickup, choose the one with the shortest distance from elevator's floor to pickup floor.
* Otherwise, when all elevators are moving in the opposite direction, choose the one with the shortest distance from the elevator's _turning point_ to the pickup floor.

_Turning point_ is defined as the floor where the elevator stops moving in the current direction. After the turning point, it might either stop or start going in the opposite direction, as explained in the previous section.

## Comparison with First Come First Serve (FCFS) approach

* new floor requests that can be served on the way to already scheduled floor requests will benefit from the higher priority and shorter waiting time
* we leverage the use of the "direction button" of the pickup request and serve this pickup request with an elevator that would travel in the desirable direction (when such elevator is available)
* when deciding on which elevator is to serve the pickup request, we always choose the one that would travel the shortest possible distance, which again results in less waiting times
* because of everything mentioned above, the elevator therefore won't travel excessive distances

## Interface

The elevator control system is exposed via the `Dispatcher` class. Instances of that class operate on an immutable `State`, the updated version of which is returned by the every publicly exposed method of that class.

An initial state to work with could be generated by the static method `createState(Int)` that accepts the number of elevators to create as a parameter.

The `Dispatcher` interface offers the following methods to operate on a `State`:

* `pickup(PickupRequest)` - schedule a pickup request and send the best suitable elevator in its direction.
  * `PickupRequest` is a data structure that defines the following interface: `pickupFloor: Int, direction: Direction`, where `Direction` is either `Direction.Up` or `Direction.Down`
* `newDestination(Int, Int)` - accepts elevator id and target floor as arguments; simulates the floor button press inside the elevator. The new destination floor is scheduled for the elevator.
* `step()` - performs the next step of the simulation: 
  * move elevators according to their route
  * stop the elevators that reached their destination and don't have further requests
  * remove pickup requests if there are suitable elevators on their floor

`State` is a data structure that offers the following interface:
* `elevators: IndexedSeq[ElevatorState]` - represents the current state of all elevators; `IndexedSeq` is used here as it offers constant lookup time and we have to look them up for pickup requests and floor schedulings
* `pendingPickupRequests: Queue[PickupRequest]` - holds non-served pickup requests; as we add them frequently, `Queue` is used here as it offers constant append time

`ElevatorState` is a data structure that represents the current status of an elevator; it's an abstract definition that is implemented in two subclasses:
* `Stopped` - represents a stopped elevator and exposes `currentFloor: Int` in its interface
* `Moving` - represents a moving elevator; exposes:
  * `currentFloor: Int`, 
  * `targetFloor: Int` that represents the destination floor in the direction where an elevator is currently moving; defined as _turning point_ above
  * `nextFloor: Option[Int]` - represents the destination floor in the opposite direction of the current movement; the elevator should move in the direction of that floor after it reaches its current destination. If `None`, elevator would stop after reaching its current destination.


All mentioned data structures are immutable.

## Purposedly ignored

The following is considered to be not as important for the exercise and is left out:
* counting how many people are currently in the elevator
* managing the evevator's capacity (how many people it would fit in)
* managing the status of the doors (open/closed)
* keeping track of which elevator has served which pickup requests


## UI

In order to ease the manual testing of the system, a simple CLI has been developed. After starting the application, the user will be offered to enter the available commands in its internal REPL. Each command will result in an updated state of the simulation that would be passed to the next iteration of the REPL.

At the very start of the simulation it asks how many elevators the user would want to run. This number is currently limited to 16.

The following commands are offered in REPL:
* schedule a pickup request
* schedule a new destination floor for a particular elevator
* execute the next step in the simulation
* print the current status of the system
* exit the REPL

## Installation / Running

SBT is required to run the project in the "development mode". Java 1.8 is required to execute the assembled JAR file. The project has been tested on OpenJDK 1.8.0_232.

The service could be run by SBT in the "development mode":
```
sbt run
```

Another possible option is to assemble the JAR file and execute it directly:

```
sbt assembly
java -jar target/scala-2.13/relayrable-elevators-assembly-0.1.0-SNAPSHOT.jar
```

The prepared JAR file could be downloaded from the releases page on GitHub: https://github.com/rgorel/relayrable-elevators/releases

