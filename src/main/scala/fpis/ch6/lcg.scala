package fpis.ch6

object lcg {
  def apply(seed: Long): Long =
    (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
}
