package org.purang.net.http

trait Header {
  require(name != null && values != null)
  val name: String
  val values: Vector[String]

  final val value: String = values.head

  override def toString: String = name + ": " + values.mkString(", ")
}

object Header {

  def apply(key: String, values: Vector[String]): Header = HeaderImpl(key, values)

  def unapply(arg: Header): Option[(String, Vector[String])] = Option((arg.name, arg.values))
}

case class HeaderImpl(name: String, values: Vector[String]) extends Header

case class HeaderValues(values: Vector[String])  {
  def `:`(name: String): Header = {
    Header(name, values)
  }

  def `,`(value: String) = HeaderValues(values :+ value)
}

object ContentType extends Function[HeaderValues, Header] {
  def apply(headerValues: HeaderValues) : Header =  "Content-Type" `:` headerValues
}

object Accept extends Function[HeaderValues, Header] {
  def apply(headerValues: HeaderValues) : Header =  "Accept" `:` headerValues
}

object TextPlain extends HeaderValues(Vector("text/plain"))
object TextHtml extends HeaderValues(Vector("text/html"))
object ApplicationJson extends HeaderValues(Vector("application/json"))
object MultipartMixed extends HeaderValues(Vector("multipart/mixed"))
