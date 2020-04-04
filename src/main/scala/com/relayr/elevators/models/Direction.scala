package com.relayr.elevators.models

sealed abstract class Direction

object Direction {
  final case object Up extends Direction
  final case object Down extends Direction
}
