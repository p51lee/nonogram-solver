import scala.io.Source
import scala.util.Using
import util.{Block, White, Black, Dummy, NemoLine, NemoPlane}

object Nemo {
  def main(args: Array[String]): Unit = {
    val filePath = args.head
    Using(Source.fromFile(filePath)) { source =>
      val lines = source.getLines().toList
      val width = lines.head.toInt
      val height = lines.tail.head.toInt
      val totInfo = lines.tail.tail
      assert(totInfo.length == width + height)
      val horizontalInfo = totInfo.dropRight(width).map( s =>
        s.split(" ").toList.map( x => x.toInt )
      )
      val verticalInfo = totInfo.drop(height).map( s =>
        s.split(" ").toList.map( x => x.toInt )
      )
      val plane = new NemoPlane(width, height, horizontalInfo, verticalInfo)
      solve(plane)
    }

    val plane2 = new NemoPlane(
      15, 15,
      List(
        List(3),
        List(5),
        List(4, 3),
        List(7),
        List(5),
        List(3),
        List(5),
        List(1, 8),
        List(3, 3, 3),
        List(7, 3, 2),
        List(5, 4, 2),
        List(8, 2),
        List(10),
        List(2, 3),
        List(6)
      ),
      List(
        List(3),
        List(4),
        List(5),
        List(4),
        List(5),
        List(6),
        List(3, 2, 1),
        List(2, 2, 5),
        List(4, 2, 6),
        List(8, 2, 3),
        List(8, 2, 1, 1),
        List(2, 6, 2, 1),
        List(4, 6),
        List(2, 4),
        List(1)
      )
    )
//    solve(plane2)
  }

  def solve(plane: NemoPlane): Unit = {
    println(plane)
    println()
    scala.io.StdIn.readLine()
    if (plane.getMatrix.map(line => line contains Dummy).reduce(_||_))
      solve(plane.next())
  }
}
