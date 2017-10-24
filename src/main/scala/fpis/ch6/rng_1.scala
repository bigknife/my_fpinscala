package fpis.ch6.rng1

import fpis.ch6._

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = lcg(seed)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (_1, rng1) = rng.nextInt
    val (_2, rng2) = rng1.nextInt
    ((_1, _2), rng2)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, rng1) => (-(Int.MinValue + 1), rng1)
      case (i, rng1) => (i.abs, rng1)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = rng.nextInt
    (i.toDouble / Double.MaxValue, rng1)
  }

  def intDouble(rng: RNG):((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG):((Double, Int), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, rng: RNG, list: (List[Int], RNG)): (List[Int], RNG) =
      if (count <= 0) list
      else {
        val (i, rng1) = rng.nextInt;
        go(count -1, rng1, (list._1 :+ i, rng1))
      }
    go(count, rng, (List[Int](),rng))
  }
}
