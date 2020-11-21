import java.io.{BufferedWriter, File, FileWriter, IOException, InputStream, PrintWriter}
import java.nio.file.Files
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.TimeZone

import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.ParserConfigurationException
import org.w3c.dom.Node
import org.xml.sax.SAXException

sealed trait Directory
case object Root extends Directory
case object Document extends Directory
case object Sentences extends Directory
case object Sentence extends Directory
case object Tokens extends Directory
case object Token extends Directory
case object Word extends Directory
case object POS extends Directory
case object OtherElement extends Directory
case object Other extends Directory


// 未使用
object Extract {
  // 外部ファイル出力
  var writer : PrintWriter= new PrintWriter(System.out)

  // POSのタグ付けした文章を格納
  var pos_tagList : List[List[(String, String)]] = List()
  var pos_tag : List[(String, String)] = List()
  // 一時保存用
  var word : String = null
  var pos : String = null

  // xmlファイルから情報を抜き出す
  def extract(outputFile : String) = try {
    // 出力ファイル設定
    if (outputFile != null) {
      writer = new PrintWriter(new BufferedWriter(new FileWriter(new File(outputFile))))
      System.out.println("-> output_tag.txt")
    }

    // 入力ファイル
    val is = Files.newInputStream(Paths.get("src/output.xml"))
    try {
      val formatter = new SimpleDateFormat("mm:ss.SSS")
      formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
      var start = System.currentTimeMillis

      System.out.println("> import output.xml")
      val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder
      val root = builder.parse(is)
      readXml(root)

      // Listを出力
      for (e <- pos_tagList) {
        writer.println(e)
      }

      var endtime = System.currentTimeMillis
      System.out.println("抽出時間 = " + formatter.format(endtime - start))
      println("finish extraction")
    } catch {
      case e@(_: ParserConfigurationException | _: IOException | _: SAXException) =>
        System.out.println(e.getMessage)
    } finally if (is != null) is.close()
  }

  // 現在の読み込んでいる階層
  var directory : Directory = Root

  // DOM(木構造)を利用して情報を取得
  private def readXml(node: Node): Unit = {
    val nodename : String = node.getNodeName

    nodename match {
      case "#text" =>
      case "xml-stylesheet" | "#document" | "root" => directory = Root
      case "document" => directory = Document
      case "sentences" => directory = Sentences
      case "sentence" => directory = Sentence //if (directory == Sentences)
      case "tokens" => directory = Tokens//if (directory == Sentence)
      case "token" =>  directory = Token//if (directory == Tokens)
      case "word" =>  directory = Word//if (directory == Token)
      case "POS" =>  directory = POS//if (directory == Token)
      //case "lemma" | "CharacterOffsetBegin" | "CharacterOffsetEnd" | "NER" | "Speaker" | "sentiment" => directory = OtherElement
      case _ => directory = Other
    }

    // 現在の階層
    // println("directory " + directory)

    var child = node.getFirstChild
    while ( {child != null}) {
      val childNodeName : String = child.getNodeName

      // 現在の階層によって処理を分ける(必要な部分のみ探索するようにする)
      directory match {
        case Document => {
          if (childNodeName == "sentences") {
            readXml(child)
            directory = Document
            child = null
          } else {
            child = child.getNextSibling
          }
        }
        case Sentences => {
          readXml(child)
          directory = Sentences
          child = child.getNextSibling
        }
        case Sentence => {
          if (childNodeName == "tokens") {
            readXml(child)
            directory = Sentence
            if (pos_tag.length != 0) {
              pos_tagList :+= pos_tag
              pos_tag = List()
              writer.println("")
            }

            child = null
          } else {
            child = child.getNextSibling
          }
        }
        case Tokens => {
          readXml(child)
          directory = Tokens
          if (word != null && pos != null) {
            pos_tag :+= (word, pos)
            word = null
            pos = null

            writer.print(" ")
          }
          child = child.getNextSibling
        }
        case Token => {
          if (childNodeName == "word" | childNodeName == "POS") {
            readXml(child)
            directory = Token
            child = child.getNextSibling
          } else {
            child = child.getNextSibling
          }
        }
        case Word =>
          if (childNodeName == "#text") {
            writer.print(child.getNodeValue)
            word = child.getNodeValue
            child = null
          } else {
            child = child.getNextSibling
          }
        case POS =>
          if (childNodeName == "#text") {
            writer.print("_" + child.getNodeValue)
            pos = child.getNodeValue
            child = null
          } else {
            child = child.getNextSibling
          }
        case _ => {
          readXml(child)
          child = child.getNextSibling
        }
      }

    }
  }
}