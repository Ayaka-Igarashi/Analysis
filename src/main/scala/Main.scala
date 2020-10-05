import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.TimeZone

import Extract.extract
import ComposeLexer.{compose, readTree}
import SpecificationAnalysis.{analysis, treeList}
import ConvertTree.{convert, tokenList, makeLeafMap}
import edu.stanford.nlp.io.IOUtils

object Main {
  // ファイル
  var inputFileName: String = null
  var txtOut: PrintWriter = null
  var xmlOut : PrintWriter = null
  /***
   *
   * @param args ファイル名
   * args(0) : 解析したい入力ファイル(input.txt)
   * args(1) : 解析結果出力ファイル(output.txt)
   * args(2) : 出力xmlファイル(output.xml)...未使用
   *
   */
  def main(args: Array[String]) = {
    // 入力ファイル
    if (args.length > 0) {
      inputFileName = args(0)
      System.out.println("inputfile: " + args(0))
    }
    // 出力ファイル
    if (args.length > 1) {
      txtOut = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
      System.out.println("txtout: " + args(1))
    }
    else txtOut = new PrintWriter(System.out)
    if (args.length > 2) {
      xmlOut = new PrintWriter(args(2))
      System.out.println("xmlout: " + args(2))
    }

    // 時間を計測
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    System.out.println("> parse_start")
    // 入力ファイルを解析する
    analysis()

    var endtime = System.currentTimeMillis
    System.out.println("解析時間 = " + formatter.format(endtime - start))
    start = System.currentTimeMillis

    System.out.println("> convert_start")
    for (t <- treeList) {
      makeLeafMap(t._1)
      tokenList = t._2
      val tag = convert(t._1)
      txtOut.println(tag)
    }

    endtime = System.currentTimeMillis
    System.out.println("変換時間 = " + formatter.format(endtime - start))
    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)
  }
}
