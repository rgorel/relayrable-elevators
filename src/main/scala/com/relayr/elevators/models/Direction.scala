package com.relayr.elevators.models

sealed abstract case class Direction(sign: Int)

object Direction {
  final object Up extends Direction(1)
  final object Down extends Direction(-1)
}
