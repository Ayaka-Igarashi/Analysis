import java.io.{BufferedOutputStream, FileOutputStream, IOException, ObjectOutputStream}
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.ObjectInputStream

import Main.pStateMap
import StateParsedStructure.nState
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object PreserveDefinition {
  // stateMapを保存する
  def preserve[A](obj: A, filename: String) = {
    try {
      val f = new FileOutputStream(filename)
      val b = new BufferedOutputStream(f)
      val out = new ObjectOutputStream(b)
      try {
        out.writeObject(obj)
      } catch {
        case e: IOException => e.printStackTrace()
      } finally {
        if (out != null) out.close()
      }
    }
  }
  // 保存したものを取り出す
  def read[A](filename: String): A = {
    var a: A = asInstanceOf[A]
    try {
      val f = new FileInputStream(filename)
      val b = new BufferedInputStream(f)
      val in = new ObjectInputStream(b)
      try {
        a = in.readObject.asInstanceOf[A]
      } catch {
        case e: IOException => e.printStackTrace()
        case e: ClassNotFoundException => e.printStackTrace()
      } finally {
        if (in != null) in.close()
      }
    }
    a
  }
}
