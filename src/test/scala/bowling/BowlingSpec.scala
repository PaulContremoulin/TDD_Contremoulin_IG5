package bowling

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec


/**
  * @author Contremoulin Paul - IG5
  */

class BowlingSpec extends FunSpec with Matchers {


  implicit def intTimes(i: Int) = new {
    def times(fn: => Bowling) = (1 to i) foreach (x => fn)
  }

  implicit def bowlingAction(game: Bowling) = new {

    def withSpare = {
      game.addPins(7).addPins(3)
    }

    def withPins(i : Int) = {
      game.addPins(i)
    }

    def withStrike = {
      game.addPins(10)
    }

  }

  describe("Bowling score"){

    it("should be 0 if 0 pin on 10 frames") {
      aBowlingWith(0) should have (
        'score(0)
      )
    }

    it("should be 20 if 1 pin on 10 frames") {
      aBowlingWith(1) should have (
        'score(20)
      )
    }

    it("should be 300 if 10 pin (strike) on 10 frames") {
      aBowlingWith(10) should have (
        'score(300)
      )
    }

    it("should be 150 if 5 pins then 5 pins (spare) on 10 frames") {

      aGameWithSpare5() should have (
        'score(150)
      )
    }

    it("should be 170 if 7 pins then 3 pins (spare) on 10 frames") {
      aGameWithSpare(7,3) should have (
        'score(170)
      )
    }

    it("should be 130 if 3 pins then  pins (spare) on 10 frames") {
      aGameWithSpare(3,7) should have (
        'score(130)
      )
    }

  }

  def aGame : Bowling = Bowling()

  def aGameWithSpare5() : Bowling = aBowlingWith(5)

  @tailrec
  private def aGameWithSpare(firstPins: Int = 7, secondPins: Int, game: Bowling = Bowling()) : Bowling = {
    if(game.isFinished) game
    else aGameWithSpare(secondPins, firstPins, game.addPins(firstPins))
  }

  @tailrec
  private def aBowlingWith(numberPins: Int, game: Bowling = Bowling()) : Bowling = {
    if(game.isFinished) game
    else aBowlingWith(numberPins, game.addPins(numberPins))
    //(1 to 2*10).foldLeft(game.addPins(numberPins)) ( (a, _) => a.addPins(numberPins) )
  }

}

