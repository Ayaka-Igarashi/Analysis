import java.nio.file.Files
import java.io.File

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.jsonSchema.Model

object TestFormatter {
  def format(filename: String) = {
    val s = new String(Files.readAllBytes(new File(filename).toPath()), "utf-8")
    val json: String = s

    val mapper = new ObjectMapper
    val model = mapper.readValue(json, classOf[Model])
    System.out.println(model.tests.get(2).output.get(0).get(0))
  }
}
