package bowling

/**
  * @author Contremoulin Paul - IG5
  */

case class Bowling(score: Int, frames: Int, rolls : Int, accruedScore: Int){

  def isFinished : Boolean = thirdOfLastFramePerformed || noBonusOnSecondOfLastFrame

  def addPins(number: Int): Bowling = {
    if(inFirstRoll()) {
      if(isStrike(number)) {
        if(isLastFrame) {
          if(consecutiveStrike) copy(bonus(number), frames, switchRoll, decrementStrikeBonus + number)
          else copy(score, frames, switchRoll, accruedScore + number)
        }
        else if(consecutiveStrike) copy(bonus(number), incrementFrame, rolls, decrementStrikeBonus + number)
        else copy(score, incrementFrame, rolls, accruedScore + number)
      }
      else if(consecutiveStrike) copy(bonus(number), frames, switchRoll, decrementStrikeBonus + number)
      else if(consecutiveSpare) copy(bonus(number), frames, switchRoll, number)
      else copy(score, frames, switchRoll, accruedScore + number)
    }
    else {
      if(isStrike(number)) copy(bonus(number), incrementFrame, switchRoll, decrementStrikeBonus + number)
      else if(isSpare(number)) copy(score, incrementFrame, switchRoll, number + accruedScore )
      else copy(score + number + accruedScore, incrementFrame, switchRoll, 0)
    }
  }

  private def noBonusOnSecondOfLastFrame : Boolean = numberOfRolls == 20 && accruedScore == 0

  private def thirdOfLastFramePerformed : Boolean = numberOfRolls >= 21

  private def inFirstRoll() : Boolean = rolls == 0

  private def isStrike(number: Int) : Boolean = number == 10

  private def isSpare(number: Int) : Boolean = number + accruedScore == 10

  private def consecutiveStrike : Boolean = accruedScore == 20

  private def consecutiveSpare : Boolean = accruedScore == 10

  private def incrementFrame : Int = frames+1

  private def switchRoll : Int = if(inFirstRoll()) 1 else 0

  private def numberOfRolls : Int = frames * 2 +rolls

  private def decrementStrikeBonus : Int = accruedScore - 10

  private def isLastFrame : Boolean = frames == 9

  private def bonus(number : Int) : Int = score + accruedScore + number

}

object Bowling {

  def apply() = new Bowling(0, 0, 0, accruedScore = 0)

}

