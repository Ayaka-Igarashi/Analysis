package com.jsonSchema

import java.util.HashMap
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
@JsonPropertyOrder(Array("code", "line", "col"))
class Error {

  @JsonProperty("code")
  @BeanProperty
  var code: String = _

  @JsonProperty("line")
  @BeanProperty
  var line: java.lang.Integer = _

  @JsonProperty("col")
  @BeanProperty
  var col: java.lang.Integer = _

  @JsonIgnore
  @BeanProperty
  var additionalProperties: Map[String, Any] = new HashMap[String, Any]()

  @JsonAnySetter
  def setAdditionalProperty(name: String, value: AnyRef) {
    this.additionalProperties.put(name, value)
  }
}
