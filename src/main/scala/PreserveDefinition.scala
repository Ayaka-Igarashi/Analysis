import java.io.{BufferedOutputStream, FileOutputStream, IOException, ObjectOutputStream}

import Main.pStateMap
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object PreserveDefinition {
  // stateMapを保存する
  def preserve() = {
    try {
      val f = new FileOutputStream("src/definition.dat")
      val b = new BufferedOutputStream(f)
      val out = new ObjectOutputStream(b)
      try {
        out.writeObject(pStateMap)
      } catch {
        case e: IOException =>
          e.printStackTrace()
      } finally {
        if (out != null) out.close()
      }
    }
  }
  // 保存したものを取り出す
  def read() = {
    import java.io.BufferedInputStream
    import java.io.FileInputStream
    import java.io.IOException
    import java.io.ObjectInputStream
    try {
      val f = new FileInputStream("src/definition.dat")
      val b = new BufferedInputStream(f)
      val in = new ObjectInputStream(b)
      try {
        pStateMap = in.readObject.asInstanceOf[ListMap[String, pState]]
      } catch {
        case e: IOException =>
          e.printStackTrace()
        case e: ClassNotFoundException =>
          e.printStackTrace()
      } finally {
        if (in != null) in.close()
      }
    }
  }
}
