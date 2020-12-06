import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import StateStructure._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Node}

import scala.collection.JavaConverters._

// HTML文章の解析、構造化する
object ParseHtml {
  var stateList: List[State] = List()
  var state: State = State(null, "", List())
  var charas: List[String] = List()

  var haveReadTrans: Boolean = false

  var htmlOut: PrintWriter = null

  def parseHtml() = {
    System.out.println("> html_parse_start")
    htmlOut = new PrintWriter(new BufferedWriter(new FileWriter(new File("src/outputHTML.txt"))))

    // htmlのparse
    val doc: Document = Jsoup.parse(new File("src/inputHTML.txt"),null)
    //htmlOut.println(doc)

    // 必要なものを取り出す
    val rootNode: Node = doc.body()
    readHtml(rootNode)
    //htmlOut.println(stateList)

    htmlOut.close()

    System.out.println("> html_parse_fin")
  }

  def readHtml(node: Node): Unit = {
    node.nodeName() match {
      case "h5" => {haveReadTrans = false; stateName(node)}
      case "p" => {
        if (!haveReadTrans) {
          val leave = getLeave(node)
          state.prev += leave
        }
      }
      case "dl" => {
        haveReadTrans = true
        trans(node)
        stateList :+= state
        htmlOut.println(state)
      }
      case _ => {
        val children: List[Node] = node.childNodes().asScala.toList

        for (child <- children) {
          readHtml(child)
        }
      }
    }
  }

  def stateName(node: Node) = {
    state = State(null, "", List())
    val children: List[Node] = node.childNodes().asScala.toList
    for (child <- children) {
      if (child.nodeName() == "dfn") {
        val stateName = getLeave(child)
        state.name = stateName.replace(" ", "_").replace("-","_")
          .replace("(", "").replace(")", "")
        val komoji: String = stateName(0).toLower + stateName.tail
        Replacement.replaceState += ("|" + stateName.replace("(", "\\(").replace(")", "\\)") + "|" + komoji.replace("(", "\\(").replace(")", "\\)"))
      }
    }
  }

  def trans(node: Node):Unit = {
    node.nodeName() match {
      case "dt" => {
        var leave = getLeave(node)
        leave = leave.replace("\n", "")
        //chara = leave
        charas :+= leave
      }
      case "dd" => {
        var leave = getLeave(node)
        leave = leave.replace("\n", "")
        //leave = Replacement.replace(leave)
        for (c <- charas) {
          state.trans :+= Trans(c, leave)
        }
        //state.trance :+= Trance(chara, leave)
        charas = List()
      }
      case _ => {
        val children: List[Node] = node.childNodes().asScala.toList
        for (child <- children) {
          trans(child)
        }
      }
    }
  }

  // nodeの葉の要素を文字列にして取得する
  def getLeave(node: Node):String = {
    var str: String = ""
    if (node.childNodeSize() == 0) {
      node.toString()
    } else {
      val children: List[Node] = node.childNodes().asScala.toList
      for (child <- children) {
        str += getLeave(child)
      }
      str
    }
  }
}
