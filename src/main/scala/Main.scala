import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.TimeZone

import Extract.extract
import ComposeLexer.{compose, readTree}
import SpecificationAnalysis.{analysis, treeList}
import ConvertTree.convert
import edu.stanford.nlp.io.IOUtils

object Main {
  var txtOut: PrintWriter = null
  /***
   *
   * @param args
   * args(0) : 解析したい入力ファイル(input.txt)
   * args(1) : 解析結果出力ファイル(output.txt)
   * args(2) : 出力xmlファイル(output.xml)...未使用
   *
   */
  def main(args: Array[String]) = {
    txtOut = new PrintWriter(System.out)
    if (args.length > 1) {
      txtOut = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
    }

    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    System.out.println("> parse_start")
    val A = args.slice(0,1)
    // 入力ファイルを解析する
    analysis(A)

    var endtime = System.currentTimeMillis
    System.out.println("解析時間 = " + formatter.format(endtime - start))
    start = System.currentTimeMillis

    System.out.println("> convert_start")
    for (t <- treeList) {
      val tag = convert(t)
      txtOut.println(tag)
    }

    endtime = System.currentTimeMillis
    System.out.println("変換時間 = " + formatter.format(endtime - start))
    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)
  }
}
