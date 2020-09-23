import java.text.SimpleDateFormat
import java.util.TimeZone

import Extract.extract
import ComposeLexer.{compose, readTree}
import SpecificationAnalysis.{analysis, treeList}
import ConvertTree.convert

object Main {
  def main(args: Array[String]) = {
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
    System.out.println("> main")

    val A = args.slice(0,3)
    analysis(A)

    var endtime = System.currentTimeMillis
    System.out.println("解析時間 = " + formatter.format(endtime - start))
    start = System.currentTimeMillis
    System.out.println("> compose")

    println(convert(treeList(0)))


    /*
    for (t <- treeList) {
      //println(t)
    }
    readTree(treeList)
*/
    endtime = System.currentTimeMillis
    System.out.println("出力時間 = " + formatter.format(endtime - start))
  }
}
