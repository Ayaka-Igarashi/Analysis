import TagStructure._
import scalafx.application.{AppHelper, JFXApp}
import scalafx.scene.Scene
import scalafx.scene.control.{TreeItem, TreeView}
import scalafx.scene.layout.{Priority, VBox}
import scalafx.scene.paint.Color

// Tagを見やすく表示するオブジェクト
object ShowTree extends JFXApp{

  // tagを見やすく表示する
  def showTree(tag_list: List[Tag]) = {
    Main.tag_list = tag_list
    super.main(Array())
  }

  val tree = new TreeView[String]() {
    vgrow = Priority.Always
    root = new TreeItem[String]("tag") {
      children = Main.tag_list.map { t =>
        nodes(t)
      }
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "ScalaFX TreeView"
    scene = new Scene {
      fill = Color.Green
      root = new VBox {
        hgrow = Priority.Always
        children = tree
      }
    }
  }

  def nodes(tag: Tag): TreeItem[String] = {
    var treeItem: TreeItem[String] = null
    tag match {
      case Node(n, list) => {
        treeItem = new TreeItem[String](n.toString)
        treeItem.children = Seq()
        list.map { l =>
          treeItem.children.add(nodes(l))
        }
      }
      case Leaf(n, Token(w, t)) => {
        treeItem = new TreeItem[String](n.toString)
        treeItem.children = Seq(new TreeItem[String](w))
      }
      case _ =>
    }

    treeItem
  }
}