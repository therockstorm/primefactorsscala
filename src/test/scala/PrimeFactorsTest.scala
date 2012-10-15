import org.scalatest.FunSuite

class PrimeFactorsTest extends FunSuite {
  def primeFactors(n: Int) = {
    if (n > 1) {
    	List(n)
    } else {
    	List[Int]()
    }
  }

  test("Factor of one is empty list") {
    assert(List[Int]() === primeFactors(1))
  }

  test("Factor of 2 is 2") {
	assert(List(2) === primeFactors(2))
  }

  test("Factor of 3 is 3") {
	assert(List(3) === primeFactors(3))
  }

  test("Factors of 4 are 2 and 2") {
	assert(List(2, 2) === primeFactors(4))
  }
}