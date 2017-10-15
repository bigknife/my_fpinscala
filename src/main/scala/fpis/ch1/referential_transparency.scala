package fpis.ch1

object RT extends App {
  val r1 = "hello,world".reverse
  val r2 = "hello,world".reverse
  val r3 = r1

  println(s"r3 == r2 ? ${r3 == r2}")
}

object NRT extends App {
  val sb = new StringBuilder
  val s1 = sb.append("hello").toString()
  val s2 = sb.append("hello").toString()
  val s3 = s1

  println(s"s3 == s2 ? ${s3 == s2}")
}