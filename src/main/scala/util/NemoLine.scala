package util

import scala.annotation.tailrec

class NemoLine(blockList: List[Block], info: List[Int], posses: List[List[Block]] = List()) {
  val possibilities: List[List[Block]] = if posses.isEmpty then preprocess() else posses

  def getBlockList: List[Block] = blockList
  def getPosses: List[List[Block]] = possibilities
  def getInfo: List[Int] = info

  def next(): NemoLine = {
    val newPosses = possibilities.filter( p => evaluate(p) )
    val newBlockList = newPosses.reduce(andOp)
    new NemoLine(newBlockList, info, newPosses)
  }

  def andOp(poss1: List[Block], poss2: List[Block]): List[Block] = {
    poss1.zip(poss2).map((block1, block2) => if block1 == block2 then block1 else Dummy)
  }

  private def evaluate(possibility: List[Block]): Boolean = {
    isValid(possibility) && evaluateSuppl(possibility, info)
  }

  private def isValid(possibility: List[Block]): Boolean = {
    blockList.zip(possibility).foldLeft(true)( (acc, blocks) => blocks._1 match {
      case Dummy => acc
      case Black => blocks._2 match {
        case Black => acc
        case White => false
      }
      case White => blocks._2 match {
        case Black => false
        case White => acc
      }
    })
  }

  private def countFirstBlack(possibility: List[Block]): Int = possibility match {
    case Black :: tail => 1 + countFirstBlack(tail)
    case _ => 0
  }

  @tailrec
  private def evaluateSuppl(possibility: List[Block], inf: List[Int]): Boolean = possibility match {
    case Nil => true
    case White :: possTail => evaluateSuppl(possTail, inf)
    case _ =>
      if (countFirstBlack(possibility) == inf.head) {
        evaluateSuppl(possibility.drop(inf.head), inf.tail)
      } else false
  }

  private def preprocess(): List[List[Block]] = {
    preprocessSuppl(blockList.length, info)
  }

  private def preprocessSuppl(length: Int, inf: List[Int]): List[List[Block]] = info.length match {
    case 0 => List(List.fill(length)(White))
    case _ => inf match {
      case Nil => List(List.fill(length)(White))
      case infHead :: infTail =>
        val tailMinLength = infTail.sum + infTail.length - 1
        val maxIndent = length - tailMinLength - infHead - 1
        List.range(0, maxIndent+1).flatMap(indent => {
          preprocessSuppl(length-indent-infHead, infTail).map( tail => {
            List.fill(indent)(White) ++ List.fill(infHead)(Black) ++ tail
          })
        })
    }
  }
}
