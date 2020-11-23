import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.TimeZone

import CommandStructure.Command
import SpecificationAnalysis.{analysis, treeList, treeList2}
import ConvertTree.{convert, makeLeafMap, tokenList, tokenList2}
import ParseHtml.{parseHtml, stateList}
import Replacement.replace_out
import StateProcessedStructure.pState
import TagStructure._
import TagToCommand.{tag_list, toCommand}
import edu.stanford.nlp.io.IOUtils

import scala.collection.immutable.ListMap

object Main {
  var pStateMap: ListMap[String, pState] = ListMap()

  // 木を表示させるための
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

    Replacement.replaceState = Replacement.replaceState.tail
    replace_out.println(Replacement.replaceState)

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
    for (i <- 29 - 1 to stateList.length - 1 - (stateList.length - 29)) {
      println(i+1)
      val stateName = stateList(i).name
      txtOut.println(i+1 + " : " + stateName)
      var trans_p: Map[String, List[Command]] = Map()

      for (j <- 0 to stateList(i).trans.length - 1) {
        val character = stateList(i).trans(j).character
        txtOut.println( "-- chara: "+ character + " --")
        // 入力ファイルを解析する
        var str: String = stateList(i).trans(j).process
        txtOut.println(str)
        str = Replacement.replace(str)
        txtOut.println("  | "+str)
        analysis(str)

        var tagList: List[Tag] = List()
        for (t <- treeList) {
          makeLeafMap(t._1)
          tokenList = t._2
          val tag = convert(t._1)
          tagList :+= tag
          txtOut.println(tag)
        }
        val commandList = TagToCommand.toCommand(tagList)
        txtOut.println("")
        for (c <- commandList) {txtOut.println(" -> " + c)}
        txtOut.println("")

        // Anything elseの時は例外の処理扱いする
        if (character == "Anything else") {
          trans_p = trans_p.withDefaultValue(commandList)
        } else {
          trans_p += (character -> commandList)
        }


        treeList = List()
        treeList2 = List()
        if (i == 29 - 1 && j == 1)ShowTree.showTree(tagList)
      }

      val state_p = pState(stateName, List(), trans_p)
      pStateMap += (stateName -> state_p)

    }
    txtOut.println(pStateMap)
//    pStateMap.get("Data_state") match {
//      case Some(pState(n,p,t)) => { txtOut.println(t("a"))}
//      case _ =>txtOut.println("eee")
//    }

    var endtime = System.currentTimeMillis
    System.out.println("時間 = " + formatter.format(endtime - start))
    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)
    IOUtils.closeIgnoringExceptions(replace_out)

  }
}
