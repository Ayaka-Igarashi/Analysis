package com.jsonSchema

import java.util.HashMap
import java.util.List
import java.util.Map
import com.fasterxml.jackson.annotation.JsonAnyGetter
import com.fasterxml.jackson.annotation.JsonAnySetter
import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonPropertyOrder
import scala.beans.{BeanProperty, BooleanBeanProperty}
//remove if not needed

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder(Array("description", "input", "output", "errors", "doubleEscaped", "initialStates", "lastStartTag"))
class TestFormat {

  @JsonProperty("description")
  @BeanProperty
  var description: String = _

  @JsonProperty("input")
  @BeanProperty
  var input: String = _

  @JsonProperty("output")
  @BeanProperty
  var output: List[List[Any]] = null

  @JsonProperty("errors")
  @BeanProperty
  var errors: List[Error] = null

  @JsonProperty("doubleEscaped")
  @BeanProperty
  var doubleEscaped: java.lang.Boolean = _

  @JsonProperty("initialStates")
  @BeanProperty
  var initialStates: List[String] = null

  @JsonProperty("lastStartTag")
  @BeanProperty
  var lastStartTag: String = _

  @BeanProperty
  var additionalProperties: Map[String, Any] = new HashMap[String, Any]()

  @JsonAnySetter
  def setAdditionalProperty(name: String, value: AnyRef) {
    this.additionalProperties.put(name, value)
  }
}