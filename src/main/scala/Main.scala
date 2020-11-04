import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.TimeZone

import SpecificationAnalysis.{analysis, treeList, treeList2}
import ConvertTree.{convert, makeLeafMap, tokenList, tokenList2}
import ParseHtml.{parseHtml, stateList}
import TagStructure._
import TagToCommand.{tag_list, toCommand}
import edu.stanford.nlp.io.IOUtils

object Main {
  var tag_list: List[Tag] = null

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
    // HTMLのパーサー
    parseHtml()

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

    System.out.println("> parse&convert_start")
    for (i <- 42 to stateList.length - 1) {
      println(i+1)
      txtOut.println(i+1 + " : " + stateList(i).name)
      for (j <- 0 to stateList(i).trance.length - 1) {
        txtOut.println( "-- chara: "+stateList(i).trance(j).character + " --")
        // 入力ファイルを解析する
        val str: String = stateList(i).trance(j).process
        txtOut.println(str)
        analysis(str)

        var tagList: List[Tag] = List()
        for (t <- treeList) {
          makeLeafMap(t._1)
          tokenList = t._2
          val tag = convert(t._1)
          tagList :+= tag
          txtOut.println(tag)
        }
        val commandList = toCommand(tagList)
        txtOut.println("")
        for (c <- commandList) {txtOut.println(" -> " + c)}
        txtOut.println("")

        treeList = List()
        treeList2 = List()
        //if (i == 31 && j == 7)ShowTree.showTree(tagList)
      }



    }
    var endtime = System.currentTimeMillis
    System.out.println("時間 = " + formatter.format(endtime - start))
    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)

  }
}
