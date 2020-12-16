import java.nio.file.Files
import java.io.File

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.jsonSchema.Model

object TestFormatter {
  def format(filename: String): Model = {
    val s = new String(Files.readAllBytes(new File(filename).toPath()), "utf-8")
    val json: String = s

    val mapper = new ObjectMapper
    mapper.enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
    val model = mapper.readValue(json, classOf[Model])
    model
  }
}
