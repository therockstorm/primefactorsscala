import org.scalatest.FunSuite

class PrimeFactorsTest extends FunSuite {
  def primeFactors(n: Int) = {
    var primes = List[Int]()
    var candidate = 2;
    var x = n

    while (x > 1) {
      while (x % candidate == 0) {
        primes = candidate :: primes
        x /= candidate
      }
      candidate += 1
    }

    primes
  }

  def of(n: Int) = {
    def primeFactors(n: Int, candidate: Int): List[Int] =
      if (n <= 1) List[Int]()
      else if (n % candidate == 0) candidate :: primeFactors(n / candidate, candidate)
      else primeFactors(n, candidate + 1)

    primeFactors(n, 2)
  }

  test("Factor of one is empty list") {
    assert(List[Int]() === primeFactors(1))
    assert(List[Int]() === of(1))
  }

  test("Factor of 2 is 2") {
    assert(List(2) === primeFactors(2))
    assert(List(2) === of(2))
  }

  test("Factor of 3 is 3") {
    assert(List(3) === primeFactors(3))
    assert(List(3) === of(3))
  }

  test("Factors of 4 are 2 and 2") {
    assert(List(2, 2) === primeFactors(4))
    assert(List(2, 2) === of(4))
  }

  test("Factors of 6 are 2 and 3") {
    assert(List(3, 2) === primeFactors(6))
    assert(List(2, 3) === of(6))
  }

  test("Factors of 8 are 2, 2, and 2") {
    assert(List(2, 2, 2) === primeFactors(8))
    assert(List(2, 2, 2) === of(8))
  }

  test("Factors of 9 are 3 and 3") {
    assert(List(3, 3) === primeFactors(9))
    assert(List(3, 3) === of(9))
  }
}