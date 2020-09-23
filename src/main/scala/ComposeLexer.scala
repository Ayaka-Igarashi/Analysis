import Extract.{pos_tag, pos_tagList}
import SpecificationAnalysis.treeList
import edu.stanford.nlp.trees.Tree

import scala.collection.JavaConverters._


object ComposeLexer {

  // オートマトンを定義
  var state : List[String] = List()
  var trans : Map[(String, String), String] = Map()
  var q0 : String = null;
  var alpha : List[String] = List()

  //var tree: Tree = null

  var statement: List[(String, String)] = pos_tag

  // 情報をもとに解析器を作る(old)
  def compose() = {
    // ここに書く
    println("start compose")
    readList(pos_tagList)
    println("finish compose")
  }
  // new
  def readTree(treeList: List[Tree]): Unit = {
    //println(treeList)
    treeList match {
      case t :: rst => {
        //tree = t
        //println(t2)
        matchSwitch(t)
        readTree(rst)
      }
      case Nil =>
    }

  }

  // switch文の木構造にマッチしたら状態を抽出する
  def matchSwitch(tree: Tree): Unit = {
    //println(tree.label().value())
    if (tree.label().value() == "VP" || tree.label().value() == "NP" || tree.label().value() == "ADJP") {
      VP_(tree)
    } else {
      val list = tree.getChildrenAsList.asScala.toList
      for (t <- list) {
        matchSwitch(t)
      }
    }
    /*
    if (tree.label().value() == "Switch") {
      println("switch")

    } else {
      println("else")
      val list = tree.getChildrenAsList.asScala.toList
      for (t <- list) {
        matchSwitch(t)
      }

    }

     */
  }

  def VP_(tree: Tree): Unit = {
    val list = tree.getChildrenAsList.asScala.toList

    var isMatch = false
    list match {
      case t1 :: t2 :: Nil => {
        t1.label().value() match {
          case "VB" | "VBP" |"NNP" | "JJ"=> {
            val v = t1.getChildrenAsList.asScala.toList
            v match {
              case s :: Nil => {
                s.label().value() match {
                  case "Switch" | "switch" => isMatch = true
                  case _ => isMatch = false
                }
              }
              case _ => isMatch = false
            }
          }
          case _ => isMatch = false
        }

        if (isMatch) {
          t2.label().value() match {
            case "PP" => {
              val l = t2.getChildrenAsList.asScala.toList
              l match {
                case s1 :: s2 :: Nil => {
                  s1.label().value() match {
                    case "IN" => /**簡略*/ isMatch = true
                    case _ => isMatch = false
                  }
                  s2.label().value() match {
                    case "NP" => {
                      isMatch = true
                      println(s2.getLeaves())
                    }
                    case _ => isMatch = false
                  }


                }
                case _ => isMatch = false
              }
            }
            case _ => isMatch = false
          }
        }


      }
      case _ => isMatch = false
    }

    //if (isMatch) println(tree)
  }
/*
  def VB_(tree: Tree): Unit = {
    val list = tree.getChildrenAsList.asScala.toList
    for (t <- list) {
      t.label().value() match {
        case "Switch" =>
        case _ =>
      }
    }
  }

  def PP_(tree: Tree): Unit = {

  }*/



  // 1つずつ文章を読み込む
  def readList(list: List[List[(String, String)]]): Unit = {
    list match {
      case e :: rst => {
        statement = e
        analysisSentence()
        readList(rst)
      }
      case Nil => return
    }
  }
  // 1文章を解析
  def analysisSentence(): Unit = {
    statement match {
      case e :: rst => {
        statement = rst
        e match {
          case ("Switch", "VB") | ("Switch", "VBP") => {
            eat("to")
            eat("the")
            print(" -> ")
            switch_()
          }
            /*
          case ("12." , "CD") => {
            state_()
          } */
          case _ => analysisSentence()
        }
      }
      case Nil =>
    }
  }

  def switch_():Unit = {
    statement match {
      case e :: rst => {
        statement = rst
        e match {
          case ("state", _) => {
            println(e._1)

            analysisSentence()
          }
          case _ => {
            print(e._1 + " ")
            switch_()
          }
        }

      }
      case Nil =>
    }
  }
/*
  def state_():Unit = {
    statement match {
      case e :: rst => {
        println("state : " + e._1)
        statement = rst
        if (e._1 == "state") {
          analysisSentence()
        } else {
          state_()
        }

      }
      case Nil =>
    }
  }
*/
  def eat(str: String): Unit = {
    statement match {
      case e :: rst => {
        if (e._1 == str) statement = rst // print(e._1 + " ");
      }
      case Nil =>
    }
  }

}

