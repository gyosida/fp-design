object infinitestreams {
  def from(n: Int): Stream[Int] = n #:: from(n+1)

  val naturals = from(0)

  val multiplesOfFour = naturals map (_ * 4)

  multiplesOfFour take 100 toList

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes = sieve(from(2))

  primes take 100 toList
}
