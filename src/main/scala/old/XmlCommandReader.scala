import java.io.IOException
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.TimeZone

import CommandStructure._
import Extract.{directory, pos, pos_tag, pos_tagList, readXml, word, writer}
import StateProcessedStructure.pState
import javax.xml.parsers.{DocumentBuilderFactory, ParserConfigurationException}
import org.w3c.dom.Node
import org.xml.sax.SAXException

import scala.collection.immutable.ListMap

object XmlCommandReader {
  var pStateMap: ListMap[String, pState] = ListMap()

  def startReading() = {
    val is = Files.newInputStream(Paths.get("src/output_d.xml"))
    try {
//      val formatter = new SimpleDateFormat("mm:ss.SSS")
//      formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
//      var start = System.currentTimeMillis

//      System.out.println("> import output.xml")
      val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder
      val root = builder.parse(is)
      readXml(root.getFirstChild)

      println(pStateMap)

//      var endtime = System.currentTimeMillis
//      System.out.println("抽出時間 = " + formatter.format(endtime - start))
//      println("finish extraction")
    } catch {
      case e@(_: ParserConfigurationException | _: IOException | _: SAXException) =>
        System.out.println(e.getMessage)
    } finally if (is != null) is.close()
  }

  def readXml(node: Node): Unit = {
    val nodename: String = node.getNodeName
    nodename match {
      case "stateMap" => {
        var state = node.getFirstChild
        while (state != null) {
          if (state.getNodeName != "#text")readXml(state)
          state = state.getNextSibling
        }
      }
      case "state" => {
        var childList: List[Node] = List()
        var child = node.getFirstChild
        while (child != null) {
          if (child.getNodeName != "#text")childList :+= child
          child = child.getNextSibling
        }
        stateNode(childList(0), childList(1), childList(2))
      }
      case _ => {println(nodename);readXml(node.getNextSibling)}
    }
  }

  def stateNode(name: Node, prev: Node, trans: Node) = {
    val stateName = name.getFirstChild.getNodeValue

    var prevCommandList: List[Command] = List()
    var prevChild = prev.getFirstChild
    while (prevChild != null) {
      if (prevChild.getNodeName != "#text") prevCommandList :+= NodeToCommand(prevChild)
      prevChild = prevChild.getNextSibling
    }
    var transList: List[(String, List[Command])] = List()
    var transChild = trans.getFirstChild
    while (transChild != null) {
      if (transChild.getNodeName != "#text") {
        var alpha = ""
        var commandList: List[Command] = List()

        var transChildChild = transChild.getFirstChild
        while (transChildChild != null) {
          if (transChildChild.getNodeName == "alpha") {
            alpha = transChildChild.getFirstChild.getNodeValue
          }
          else if (transChildChild.getNodeName == "command_list") {
            var comListChild = transChildChild.getFirstChild
            while (comListChild != null) {
              if (comListChild.getNodeName != "#text") commandList :+= NodeToCommand(comListChild)
              comListChild = comListChild.getNextSibling
            }
          }
          transChildChild = transChildChild.getNextSibling
        }
        transList :+= (alpha, commandList)
      }

      transChild = transChild.getNextSibling
    }
    pStateMap += (stateName -> pState(stateName, prevCommandList, transList))
  }

  def NodeToCommand(node: Node): Command = {
    var child = node.getFirstChild.getNextSibling
    val comname = child.getFirstChild.getNodeValue
    var list: List[Node] = List()
    child = child.getNextSibling
    while (child != null) {
      if (child.getNodeName != "#text") list :+= child
      child = child.getNextSibling
    }
    comname match {
      case "Flush" => Flush()
      case "Start" => Start()
      case "Treat" => Treat()
      case "Switch" => Switch(list(0).getFirstChild.getNodeValue)
      case "Reconsume" =>Reconsume(list(0).getFirstChild.getNodeValue)
      case "Consume" =>Consume(list(0).getFirstChild.getNodeValue)
      case "Emit" =>Emit(list(0).getFirstChild.getNodeValue)
      case "Error" =>Error(list(0).getFirstChild.getNodeValue)
      case "Create" =>Create(list(0).getFirstChild.getNodeValue)
      case "Ignore" =>Ignore(list(0).getFirstChild.getNodeValue)
      case "Set" =>Set(list(0).getFirstChild.getNodeValue, list(1).getFirstChild.getNodeValue)
      case "Append" =>Append(list(0).getFirstChild.getNodeValue, list(1).getFirstChild.getNodeValue)
      case "Multiply" =>Multiply(list(0).getFirstChild.getNodeValue, list(1).getFirstChild.getNodeValue)
      case "Add" =>Add(list(0).getFirstChild.getNodeValue, list(1).getFirstChild.getNodeValue)
      case "If" =>{
        var trueList: List[Command] = List()
        var trueChild = list(1).getFirstChild
        while (trueChild != null) {
          if (trueChild.getNodeName != "#text") trueList :+= NodeToCommand(trueChild)
          trueChild = trueChild.getNextSibling
        }
        var falseList: List[Command] = List()
        var falseChild = list(1).getFirstChild
        while (falseChild != null) {
          if (falseChild.getNodeName != "#text") falseList :+= NodeToCommand(falseChild)
          falseChild = falseChild.getNextSibling
        }
        If(nodeToBool(list(0).getFirstChild.getNextSibling),trueList,falseList)
      }
      case _ => If(null,null,null)
    }
  }

  def nodeToBool(node: Node):Bool = {
    var child = node.getFirstChild.getNextSibling
    val boolname = child.getFirstChild.getNodeValue
    var list: List[Node] = List()
    child = child.getNextSibling
    while (child != null) {
      if (child.getNodeName != "#text") list :+= child
      child = child.getNextSibling
    }
    boolname match {
      case "And" =>And(nodeToBool(list(0)), nodeToBool(list(1)))
      case "Or" =>Or(nodeToBool(list(0)), nodeToBool(list(1)))
      case "Not" =>Not(nodeToBool(list(0)))
      case "IsEqual" =>IsEqual(list(0).getFirstChild.getNodeValue, list(1).getFirstChild.getNodeValue)
      case "IsExist" =>IsExist(list(0).getFirstChild.getNodeValue)
      case "UNDEF" =>UNDEF(list(0).getFirstChild.getNodeValue)
      case _ =>UNDEF("")
    }
  }
}
