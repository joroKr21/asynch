package org.purang.net.http

/**
  * Created by ppurang on 08.05.17.
  */
package object implicits {

  private object NoopRequestModifier extends RequestModifier {
    override def modify: (Request) => Request = req => req
  }

  implicit val noop: RequestModifier = NoopRequestModifier

  def responseToString(response: AResponse): String = response match {
    case (_,_,Some(x),_) =>"""%s%n%n%s""".format(incompleteResponseToString(response), x)
    case _ => incompleteResponseToString(response)
  }

  def incompleteResponseToString(response: AResponse) : String = """%s%n%s""".format(response._1, response._2.mkString("\n"))

  implicit object MethodToString extends Function[Method, String] {
    def apply(method: Method) = method.toString
  }

  implicit def scalaIterableToHeaderValues(values: Iterable[String]): HeaderValues = {
    HeaderValues(Vector[String]() ++ values)
  }

  implicit def stringToHeaderValue(value: String): HeaderValues = HeaderValues(Vector(value))

  implicit def headerToVector(header: Header): Vector[Header] = Vector[Header](header)

  implicit def headersToString(headers: Vector[Header]): String = headers.mkString("\n")

  implicit def stringToRequest(url: String): Request = Request(url)

  implicit def responseSuccessFunctionToTuple[T](f: (Status, Headers, Body, Request) => T ): ((Status, Headers, Body, Request)) => T = f.tupled

  implicit def responseFailureFunctionToTuple[T](f: (Throwable, Request) => T ): ((Throwable, Request)) => T = f.tupled

}
