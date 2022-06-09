package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TFSpec extends AnyFlatSpec with Matchers {
  "The first result" should "equal 25" in {
    TF.res1 shouldEqual 25
  }

  "The log" should "log stuff?" in {
    TF.res2 shouldEqual (25, Vector("res2: 125", "res: 25"))
  }
}
