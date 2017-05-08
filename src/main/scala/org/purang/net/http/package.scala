package org.purang.net

import scalaz.\/, scalaz.syntax.either._

package object http {

  //type aliases
  type Status = Int
  type Headers = Vector[Header]
  type Body = Option[String]

  type FailedRequest =  (Throwable, Request)

  type AResponse = (Status, Headers, Body, Request)

  type or[+E, +A] = \/[E, A]

  type ExecutedRequest = FailedRequest or AResponse

  type NonBlockingExecutedRequest = scalaz.concurrent.Task[AResponse]

  trait NonBlockingExecutor extends (Timeout => Request => NonBlockingExecutedRequest)

  type ExecutedRequestHandler[T] = (ExecutedRequest => T)

  //types
  implicit class Url(val url: String) extends AnyVal {
    override def toString(): String = url
  }

  object Url {
    implicit def unapply(url: Url): String = url.url
  }

  implicit class Timeout(val timeout: Long) extends AnyVal {
    def +(delta: Long): Long = timeout + delta
  }
  object Timeout {
    implicit def unapply(timeout: Timeout): Long = timeout.timeout
  }

  //debug handling
  private lazy val property: String = System.getProperty("asynch.debug")

  private[http] def debug(msg: => String) = {
    if (property != null && property.toBoolean) {
      println("[ASYNCH] " + msg + "[/ASYNCH]")
    }
  }

}
