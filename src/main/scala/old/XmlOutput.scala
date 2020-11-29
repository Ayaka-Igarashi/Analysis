import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.ParserConfigurationException
import java.io.File

import StateProcessedStructure.pState
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.ParserConfigurationException
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerConfigurationException
import javax.xml.transform.TransformerException
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import org.w3c.dom.Element

import scala.collection.immutable.ListMap
import CommandStructure._

object XmlOutput {
  var document: Document = null

  def makeDocumentInstance() = {
    // Documentインスタンスの生成
    var documentBuilder: DocumentBuilder = null
    try documentBuilder = DocumentBuilderFactory.newInstance.newDocumentBuilder
    catch {
      case e: ParserConfigurationException =>
        e.printStackTrace()
    }
    document = documentBuilder.newDocument
  }

  def makeXml(stateMap: ListMap[String, pState]) = {
    if (document == null) {
      makeDocumentInstance()
    }
    // XML文書の作成
    xml(stateMap)

    // XMLファイルの作成
    write()
  }

  def xml(stateMap: ListMap[String, pState]) = {
    val stateMapElement: Element = document.createElement("stateMap")
    for (state <- stateMap) {
      state._2 match {
        case pState(name, prev, trans) => {
          val stateElement: Element = document.createElement("state")
          val nameElement: Element = document.createElement("name")
          nameElement.appendChild(document.createTextNode(name))
          stateElement.appendChild(nameElement)

          val prevElement: Element = document.createElement("prev")
          for (c <- commandToElement(prev)) prevElement.appendChild(c)
          stateElement.appendChild(prevElement)

          val transElement: Element = document.createElement("trans")
          for (t <- trans) {
            val l_t: Element = document.createElement("l_t")
            val a: Element = document.createElement("alpha")
            a.appendChild(document.createTextNode(t._1))
            val l_t2: Element = document.createElement("command_list")
            for (c <- commandToElement(t._2)) l_t2.appendChild(c)
            l_t.appendChild(a)
            l_t.appendChild(l_t2)
            transElement.appendChild(l_t)
          }
          stateElement.appendChild(transElement)
          stateMapElement.appendChild(stateElement)
        }
        case _ =>
      }
    }
    document.appendChild(stateMapElement)
  }

  def commandToElement(comList: List[Command]): List[Element] = {
    var elementList: List[Element] = List()
    for (com <- comList) {
      val l: Element = document.createElement("l")
      val c1: Element = document.createElement("command_name")
      val c2: Element = document.createElement("command_content")
      val c2_2: Element = document.createElement("command_content")
      val c2_3: Element = document.createElement("command_content")
      com match {
        case Flush() => {
          c1.appendChild(document.createTextNode("Flush"))
        }
        case Treat() => c1.appendChild(document.createTextNode("Treat"))
        case Start() => c1.appendChild(document.createTextNode("Start"))
        case Switch(one) => {
          c1.appendChild(document.createTextNode("Switch"))
          c2.appendChild(document.createTextNode(one))
        }
        case Reconsume(one) => {
          c1.appendChild(document.createTextNode("Reconsume"))
          c2.appendChild(document.createTextNode(one))
        }
        case Consume(one) => {
          c1.appendChild(document.createTextNode("Consume"))
          c2.appendChild(document.createTextNode(one))
        }
        case Emit(one) => {
          c1.appendChild(document.createTextNode("Emit"))
          c2.appendChild(document.createTextNode(one))
        }
        case Error(one) => {
          c1.appendChild(document.createTextNode("Error"))
          c2.appendChild(document.createTextNode(one))
        }
        case Create(one) => {
          c1.appendChild(document.createTextNode("Create"))
          c2.appendChild(document.createTextNode(one))
        }
        case Ignore(one) => {
          c1.appendChild(document.createTextNode("Ignore"))
          c2.appendChild(document.createTextNode(one))
        }
        case Set(one, two) => {
          c1.appendChild(document.createTextNode("Set"))
          c2.appendChild(document.createTextNode(one))
          c2_2.appendChild(document.createTextNode(two))
        }
        case Append(one, two) => {
          c1.appendChild(document.createTextNode("Append"))
          c2.appendChild(document.createTextNode(one))
          c2_2.appendChild(document.createTextNode(two))
        }
        case Multiply(one, two) => {
          c1.appendChild(document.createTextNode("Multiply"))
          c2.appendChild(document.createTextNode(one))
          c2_2.appendChild(document.createTextNode(two))
        }
        case Add(one, two) => {
          c1.appendChild(document.createTextNode("Add"))
          c2.appendChild(document.createTextNode(one))
          c2_2.appendChild(document.createTextNode(two))
        }
        case If(one, two, three) => {
          c1.appendChild(document.createTextNode("If"))
          c2.appendChild(boolToElement(one))
          for (c <- commandToElement(two)) c2_2.appendChild(c)
          for (c <- commandToElement(three)) c2_3.appendChild(c)
        }
        case _ =>
      }
      l.appendChild(c1)
      if (c2.hasChildNodes) l.appendChild(c2)
      if (c2_2.hasChildNodes) l.appendChild(c2_2)
      if (c2_3.hasChildNodes) l.appendChild(c2_3)
      elementList :+= l
    }
    elementList
  }

  def boolToElement(bool: Bool): Element = {
    val boolElement: Element = document.createElement("bool")
    val c1: Element = document.createElement("bool_name")
    val c2: Element = document.createElement("content")
    val c2_2: Element = document.createElement("content")
    bool match {
      case And(b1, b2) => {
        c1.appendChild(document.createTextNode("And"))
        c2.appendChild(boolToElement(b1))
        c2_2.appendChild(boolToElement(b2))
      }
      case Or(b1, b2) => {
        c1.appendChild(document.createTextNode("Or"))
        c2.appendChild(boolToElement(b1))
        c2_2.appendChild(boolToElement(b2))
      }
      case Not(b) => {
        c1.appendChild(document.createTextNode("Not"))
        c2.appendChild(boolToElement(b))
      }
      case IsEqual(a, b) => {
        c1.appendChild(document.createTextNode("IsEqual"))
        c2.appendChild(document.createTextNode(a))
        c2_2.appendChild(document.createTextNode(b))
      }
      case IsExist(a) => {
        c1.appendChild(document.createTextNode("IsExist"))
        c2.appendChild(document.createTextNode(a))
      }
      case UNDEF(str) => {
        c1.appendChild(document.createTextNode("UNDEF"))
        c2.appendChild(document.createTextNode(str))
      }
      case _ =>
    }
    boolElement.appendChild(c1)
    if (c2.hasChildNodes) boolElement.appendChild(c2)
    if (c2_2.hasChildNodes) boolElement.appendChild(c2_2)
    boolElement
  }

  def write(): Boolean = {
    val file: File = new File("src/output_d.xml")
    // Transformerインスタンスの生成
    var transformer: Transformer = null
    try {
      val transformerFactory = TransformerFactory.newInstance
      transformer = transformerFactory.newTransformer
    } catch {
      case e: TransformerConfigurationException =>
        e.printStackTrace()
        return false
    }

    // Transformerの設定
    transformer.setOutputProperty("indent", "yes") //改行指定
    transformer.setOutputProperty("encoding", "Shift_JIS") // エンコーディング

    // XMLファイルの作成
    try transformer.transform(new DOMSource(document), new StreamResult(file))
    catch {
      case e: TransformerException =>
        e.printStackTrace()
        return false
    }
    return true
  }
}
