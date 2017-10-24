package fpis.ch6.state1

/*
object state {
  type State[S, +A] = S => (A, S)
}
 */

final case class[S +A](run: S => (A, S))
