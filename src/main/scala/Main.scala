import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.{Scanner, TimeZone}
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectOutputStream

import scala.collection.JavaConverters._
import CommandStructure.Command
import SpecificationAnalysis.analysis
import ConvertTree.{convert, corefMap, makeLeafMap, tokenList, tokenList2}
import Environment.{Env, StateVal, endOfFileToken}
import Implement.interpret
import ParseHtml.{parseHtml, stateList}
import Replacement.replace_out
import StateParsedStructure.{nState, nTrans}
import StateProcessedStructure.pState
import TagStructure._
import TagToCommand.{tag_list, toCommand}
import edu.stanford.nlp.coref.data.CorefChain
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.trees.Tree
import org.jsoup.parser.HtmlTreeBuilder
import old.Test

import scala.collection.immutable.ListMap

object Main {
  var pStateMap: ListMap[String, pState] = ListMap()
  var nStateList: List[nState] = List()

  // 木を表示させるための
  var tag_list: List[Tag] = null

  // ファイル
  var inputFileName: String = null
  var txtOut: PrintWriter = null
  var txtOut2 : PrintWriter = null
  var txtOut3 : PrintWriter = null
  var txtOut4 : PrintWriter = null

  var testNumber: Int = 1
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

    val scanner = new Scanner(System.in)

    if (false) {
      print("> parse start?(put yes or no) : ")
      val scan = scanner.next()
      if (scan == "yes"||scan == "y") {
        // 出力ファイル
        if (args.length > 1) {
          txtOut = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
          System.out.println("txtout: " + args(1))
        } else txtOut = new PrintWriter(System.out)

        parse(1, 79)
        PreserveDefinition.preserve[List[nState]](nStateList, "src/parsed.dat")
        tagConvert()
      }
    } else if (true) {
      // 出力ファイル
      if (args.length > 1) {
        txtOut = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
        System.out.println("txtout: " + args(1))
      } else txtOut = new PrintWriter(System.out)
      txtOut3 = new PrintWriter("src/output3.txt")

      // 取り出す
      nStateList = PreserveDefinition.read[List[nState]]("src/parsed.dat")
      tagConvert()

      //implement("<abar d=kl rt=hhh>tyu</huj>", "Data_state")
    } else {
      if (args.length > 2) {
        txtOut2 = new PrintWriter(args(2))
        System.out.println("txtOut2: " + args(2))
      }
      txtOut3 = new PrintWriter("src/output3.txt")

      pStateMap = PreserveDefinition.read[ListMap[String, pState]]("src/definition.dat")
      //writeDefinition(txtOut2)
      implement("<abar d=kl rt=hhh>tyu</huj>", "Data_state", null)
    }

    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(txtOut)
    IOUtils.closeIgnoringExceptions(txtOut2)
    IOUtils.closeIgnoringExceptions(txtOut3)
    IOUtils.closeIgnoringExceptions(replace_out)
  }

  def implement(input: String, initialState: String, lastStartTagName: String): Env = {

    if (input.contains("\r")) txtOut4.println("main:: r")
    var replacedInput = input.replaceAll("\u000d\u000a", "\u000a")
    replacedInput = replacedInput.replaceAll("\u000d", "\u000a")
    replacedInput = replacedInput.replaceAll("\r\n", "\n")
    replacedInput = replacedInput.replaceAll("\r", "\n")

    var env: Env = new Env()
    env.setInputText(replacedInput)
    val length = env.inputText.length
    env.setNextState(StateVal(initialState))
    env.lastStartTagName = lastStartTagName
    var i = 1
    txtOut3.println("num :" + testNumber + ";")
    testNumber += 1
    txtOut3.println("input : " + env.inputText + "\n")
    while (!env.emitTokens.contains(endOfFileToken()) && i <= length * 2 + 10) {
      txtOut3.println(i + " : ===============================================")
      env = interpret(env, pStateMap)
      txtOut3.println("")
      Environment.printEnv(env, txtOut3, i)
      txtOut3.println("|\nV\n")
      i = i + 1
    }
    env
  }

  def combineCharacterToken(tokens: List[Environment.Token]): List[Environment.Token] = {
    var newTokens: List[Environment.Token] = List()
    var combinedCharacterToken: Environment.characterToken = Environment.characterToken("")
    for (t <- tokens) {
      t match {
        case Environment.characterToken(c) => {
          combinedCharacterToken match {
            case Environment.characterToken(str) => combinedCharacterToken = Environment.characterToken(str + c)
          }
        }
        case _ => {
          combinedCharacterToken match {
            case Environment.characterToken("") => newTokens :+= t
            case Environment.characterToken(str) => {
              newTokens :+= combinedCharacterToken
              combinedCharacterToken = Environment.characterToken("")
              newTokens :+= t
            }
          }
        }
      }
    }
    newTokens
  }

  // tagにする
  def parse(begin: Int, end: Int) = {
    // 時間を計測
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    // HTMLのパーサー
    parseHtml()

    // 状態名の置き換えのための処理
    //Replacement.replaceState = Replacement.replaceState.tail
    replace_out.println(Replacement.replaceState)

    System.out.println("> parse_start")
    for (i <- begin - 1 to stateList.length - 1 - (stateList.length - end)) {
      println(i+1)
      val stateName = stateList(i).name
      val (prevReplacedStr, prevcoref, prevTreeList) = parseStatementToTag(stateList(i).prev)

      var trans_n: List[nTrans] = List()
      for (j <- 0 to stateList(i).trans.length - 1) {
        val character = stateList(i).trans(j).character
        // 入力ファイルを解析する
        var str: String = stateList(i).trans(j).process
        val (replacedStr, coref, tree_List) = parseStatementToTag(str)
        trans_n :+= nTrans(character, (str, replacedStr, coref, tree_List))
      }
      val state_n = nState(stateName, (stateList(i).prev, prevReplacedStr, prevcoref, prevTreeList), trans_n)
      nStateList :+= state_n
    }

    var endtime = System.currentTimeMillis
    System.out.println("parse time = " + formatter.format(endtime - start))
  }

  // Commandにする
  def tagConvert() = {
    // 時間を計測
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))

    System.out.println("> convert_start")

    val list = nStateList//List(nStateList(31))

    var i = 1
    for (n_state <- list) {
      val stateName = n_state.name
      txtOut.println(i + " : " + stateName)
      val (pstr, rpstr, prevCoref, prevTagList) = n_state.prev
      txtOut.println(pstr)
      txtOut.println("  | "+rpstr)
      txtOut.println(prevCoref)
      val prevCommand = tagToCommand(prevTagList)

      var trans_p: List[(String, List[Command])] = List()
      for (n_trans <- n_state.trans) {
        val character = n_trans.character
        txtOut.println( "-- chara: "+ character + " --")
        val (str, rstr, coref, tagList) = n_trans.process

        txtOut.println(str)
        txtOut.println("  | "+rstr)
        txtOut.println(coref)
        val commandList = tagToCommand(tagList)
        trans_p :+= (character, commandList)
        //if (i == 11 - 1 && j == 8)ShowTree.showTree(tagList)
      }
      val state_p = pState(stateName, prevCommand, trans_p)
      pStateMap += (stateName -> state_p)

      //println(Implement.characterMatching("A", trans_p))

      i = i + 1
    }

    txtOut.println(pStateMap)
    PreserveDefinition.preserve(pStateMap, "src/definition.dat")

    var endtime = System.currentTimeMillis
    System.out.println("convert time = " + formatter.format(endtime - start))
  }

  var uniqueId = 0
  def parseStatementToTag(str: String): (String, List[List[(Integer, CorefChain)]], List[Tag]) = {
    val newStr = Replacement.replace(str)
    var (coref, tree_List, depMapList) = analysis(newStr)

    var tagList: List[Tag] = List()
    var i = 0
    for (t <- tree_List) {
      var corefIdx = Map[Int, Int]().withDefaultValue(-1)
      for (c <- coref(i)) {
        val order = c._2.getMentionsInTextualOrder.asScala.toList
        for (o <- order) {
          //println((o.startIndex-1) + " ~ " + (o.endIndex-2))
          corefIdx ++= (o.startIndex - 1 to o.endIndex - 2).toList.map(n => (n, o.corefClusterID))}
      }
      ConvertTree.corefMap = corefIdx
      ConvertTree.depMap = depMapList.head.withDefaultValue(Set())
      depMapList = depMapList.tail

      makeLeafMap(t._1)
      tokenList = t._2
      var tag = convert(t._1)
      tag = ConvertTree.repairLeave(tag)
      tagList :+= tag
      i += 1
    }
    uniqueId += 1

    (newStr, coref, tagList)
  }

  def tagToCommand(tagList: List[Tag]): List[Command] = {
    for (tag <- tagList) txtOut.println(tag)
    val commandList = TagToCommand.toCommand(tagList)
    txtOut.println("")
    for (c <- commandList) {txtOut.println(" -> " + c)}
    txtOut.println("")

    commandList
  }

  def writeDefinition(writer: PrintWriter) = {
    for (p <- pStateMap) {
      writer.println("======== " + p._1 + " ========")
      writer.println("name : " + p._2.name)
      writer.println("prev : " + p._2.prev)
      for (t <- p._2.trans) {
        writer.println("character : " + t._1)
        writer.println("  command : " + t._2)
        writer.println("")
      }
      writer.println("")
    }
  }
}
