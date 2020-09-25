import java.text.SimpleDateFormat
import java.util.TimeZone

import Extract.extract
import ComposeLexer.{compose, readTree}
import SpecificationAnalysis.{analysis, treeList}
import ConvertTree.convert

object Main {
  /***
   *
   * @param args
   * args(0) : 解析したい入力ファイル(input.txt)
   * args(1) : 解析結果出力ファイル(output.txt)
   * args(2) : 出力xmlファイル(output.xml)...未使用
   *
   */
  def main(args: Array[String]) = {
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    System.out.println("> parse_start")
    val A = args.slice(0,3)
    // 入力ファイルを解析する
    analysis(A)

    var endtime = System.currentTimeMillis
    System.out.println("解析時間 = " + formatter.format(endtime - start))
    start = System.currentTimeMillis

    System.out.println("> convert_start")
    for (t <- treeList) {
      val tag = convert(t)
      println(tag)
    }

    endtime = System.currentTimeMillis
    System.out.println("変換時間 = " + formatter.format(endtime - start))
  }
}
