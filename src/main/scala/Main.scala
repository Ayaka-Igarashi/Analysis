import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectOutputStream

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
  var txtOut2 : PrintWriter = null
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

    if (false) {
      // 出力ファイル
      if (args.length > 1) {
        txtOut = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
        System.out.println("txtout: " + args(1))
      }
      else txtOut = new PrintWriter(System.out)

      parseAndConvert(1, 79)
    }
    else {
      if (args.length > 2) {
        txtOut2 = new PrintWriter(args(2))
        System.out.println("txtOut2: " + args(2))
      }

      //XmlCommandReader.startReading()
      //pStateMap = XmlCommandReader.pStateMap
      PreserveDefinition.read()
      writeDefinition()
    }

    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)
    IOUtils.closeIgnoringExceptions(txtOut2)
    IOUtils.closeIgnoringExceptions(replace_out)
  }

  def parseAndConvert(begin: Int, end: Int) = {
    // 時間を計測
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    // HTMLのパーサー
    parseHtml()

    // 状態名の置き換えのための処理
    Replacement.replaceState = Replacement.replaceState.tail
    replace_out.println(Replacement.replaceState)

    System.out.println("> parse&convert_start")
    for (i <- begin - 1 to stateList.length - 1 - (stateList.length - end)) {
      println(i+1)
      val stateName = stateList(i).name
      txtOut.println(i+1 + " : " + stateName)

      val prevCommand = statementToCommand(stateList(i).prev)

      var trans_p: List[(String, List[Command])] = List()

      for (j <- 0 to stateList(i).trans.length - 1) {
        val character = stateList(i).trans(j).character
        txtOut.println( "-- chara: "+ character + " --")
        // 入力ファイルを解析する
        var str: String = stateList(i).trans(j).process
        val commandList = statementToCommand(str)
        trans_p :+= (character, commandList)
        //if (i == 11 - 1 && j == 8)ShowTree.showTree(tagList)
      }

      val state_p = pState(stateName, prevCommand, trans_p)
      pStateMap += (stateName -> state_p)

      //println(Implement.characterMatching("A", trans_p))
    }

    txtOut.println(pStateMap)
    //XmlOutput.makeXml(pStateMap)
    PreserveDefinition.preserve()

    var endtime = System.currentTimeMillis
    System.out.println("時間 = " + formatter.format(endtime - start))
  }

  def statementToCommand(str: String): List[Command] = {
    txtOut.println(str)
    val newStr = Replacement.replace(str)
    txtOut.println("  | "+newStr)
    analysis(newStr)

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
    treeList = List()
    treeList2 = List()

    commandList
  }

  def writeDefinition() = {
    for (p <- pStateMap) {
      txtOut2.println("======== " + p._1 + " ========")
      txtOut2.println("name : " + p._2.name)
      txtOut2.println("prev : " + p._2.prev)
      for (t <- p._2.trans) {
        txtOut2.println("character : " + t._1)
        txtOut2.println("  command : " + t._2)
        txtOut2.println("")
      }
      txtOut2.println("")
    }
  }
}
