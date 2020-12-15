import org.scalatest.FunSuite

class Test extends FunSuite {
  test("CubeCalculator.cube") {

    TestFormatter.format("src/test/testFile/contentModelFlags.test")
    assert(1+2 === 3)
  }
  test("a.cube") {
    assert(1+2 === 4)
  }
}
