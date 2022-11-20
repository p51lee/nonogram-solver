package util

class NemoPlane(
                 width: Int,
                 height: Int,
                 horizontalInfos: List[List[Int]],
                 verticalInfos: List[List[Int]],
                 horizontalNemoLines: List[NemoLine] = List(),
                 verticalNemoLines: List[NemoLine] = List(),
                 blockMatrix: Array[Array[Block]] = Array()
               ) {
  val blockMat: Array[Array[Block]] = if blockMatrix.isEmpty
  then Array.fill(height)(Array.fill(width)(Dummy)) else blockMatrix

  val horizontalLines: List[NemoLine] = if horizontalNemoLines.isEmpty
  then blockMat.zip(horizontalInfos).map(
      (blockArray, info) => new NemoLine(blockArray.toList, info)
    ).toList else horizontalNemoLines

  val verticalLines: List[NemoLine] = if verticalNemoLines.isEmpty
  then List.range(0, width).map(index =>
      new NemoLine(
        blockMat.map(blockArray => blockArray(index)).toList,
        verticalInfos(index)
      )
    ) else verticalNemoLines

  def getMatrix: Array[Array[Block]] = blockMat

  def next(): Option[NemoPlane] = {
    //    println("next!")
    val tempHorizontalLines = horizontalLines.flatMap(line => line.next())
    val tempVerticalLines = verticalLines.flatMap(line => line.next())
    if tempHorizontalLines.length != height || tempVerticalLines.length != width
    then None
    else {
      tempHorizontalLines.zipWithIndex.foreach { case (line, rowIndex) =>
        line.getBlockList.zipWithIndex.foreach { case (block, colIndex) =>
          if (block != Dummy) blockMat(rowIndex)(colIndex) = block
        }
      }

      tempVerticalLines.zipWithIndex.foreach { case (line, colIndex) =>
        line.getBlockList.zipWithIndex.foreach { case (block, rowIndex) =>
          if (block != Dummy) blockMat(rowIndex)(colIndex) = block
        }
      }

      val newHorizontalLines = blockMat.zip(horizontalInfos).map(
        (blockArray, info) => new NemoLine(blockArray.toList, info)
      ).toList

      val newVerticalLines = List.range(0, width).map(index =>
        new NemoLine(
          blockMat.map(blockArray => blockArray(index)).toList,
          verticalInfos(index)
        )
      )

      Some(new NemoPlane(
        width,
        height,
        horizontalInfos,
        verticalInfos,
        newHorizontalLines,
        newVerticalLines,
        blockMat
      ))
    }
  }

  override def toString: String = {
    blockMat.map(blockLine =>
      blockLine.foldLeft("")((acc, block) => block match {
        case Black => acc.concat("⬛")
        case White => acc.concat("⬜")
        case Dummy => acc.concat("\uD83E\uDD2A")
      })
    ).mkString("\n")
  }
}
