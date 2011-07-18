package org.purang.net

package http

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{GivenWhenThen, FeatureSpec}
import scalaz._
import Scalaz._


/**
 *
 * @author Piyush Purang
 */

class ExecutorSpec extends FeatureSpec with GivenWhenThen with ShouldMatchers {


  val bodyOnly: (Status, Headers, Body, Request) => String =
    (status: Status, headers: Headers, body: Body, req: Request) => body.getOrElse("")

  def responseFailureToString(t: Throwable, req: Request): String = throwableToString(t)

  /**
   * The following has been lifted from sbt/xsbt.
   */
  def throwableToString(t: Throwable): String = {
    def isSbtClass(name: String) = name.startsWith("sbt") || name.startsWith("xsbt")

    def trimmed(throwable: Throwable, d: Int = 50): String = {
      require(d >= 0)
      val b = new StringBuilder()
      def appendStackTrace(t: Throwable, first: Boolean) {
        val include: StackTraceElement => Boolean =
          if (d == 0)
            element => !isSbtClass(element.getClassName)
          else {
            var count = d - 1
            (_ => {
              count -= 1; count >= 0
            })
          }
        def appendElement(e: StackTraceElement) {
          b.append("\tat ")
          b.append(e)
          b.append('\n')
        }
        if (!first)
          b.append("Caused by: ")
        b.append(t)
        b.append('\n')
        val els = t.getStackTrace()
        var i = 0
        while ((i < els.size) && include(els(i))) {
          appendElement(els(i))
          i += 1
        }
      }
      appendStackTrace(throwable, true)
      var c = throwable
      while (c.getCause() != null) {
        c = c.getCause()
        appendStackTrace(c, false)
      }
      b.toString()
    }

    trimmed(t);
  }


  def printResponse(executedRequest: ExecutedRequest) =
    println(executedRequest.fold(responseFailureToString, bodyOnly))

  import org.purang.net.http.ning._


  feature("executor") {

    scenario("executes a request") {
      given("a request")
      val url = "http://www.google.com"
      val headers = ("Accept" `:` "application/json" ++ "text/html" ++ "text/plain") ++
              ("Cache-Control" `:` "no-cache") ++ ("Content-Type" `:` "text/plain")

      when("it is executed")
      then("status is not -1")
      (url >> headers) ~> ((x: ExecutedRequest) => x.fold(
        t => {t._1.printStackTrace ;-1},
        (status: Status, headers: Headers, body: Body, req: Request) => status
      )) should be(302)

    }

    scenario("executes a more complicated request") {
      given("a request")
      val url = "http://www.google.com"
      val headers = ("Accept" `:` "application/json" ++ "text/html" ++ "text/plain") ++
              ("Cache-Control" `:` "no-cache") ++ ("Content-Type" `:` "text/plain")

      when("it is executed")
      (HEAD > url >> headers) ~> ((x: ExecutedRequest) => { x.fold(
        (t:Throwable, _:Request) => t.printStackTrace,
        x => {
         x match  {
           case (302, rheaders, _, _) =>
             (GET > rheaders.filter(_.name.equals("Location"))(0).value >> headers) ~>
                     { _.fold(t => fail(t._1), _ match {case (status,_,_,_) => status should be(200)})}
                     //printResponse
           case x => fail(x)
         }
       })
      })
    }

   scenario("executes a request after modifying it and returns a value") {
      given("a request")
      val url = "http://www.google.com"
      val contentType = ContentType(ApplicationJson)

      implicit val reqWithApplicationJson : RequestModifier = (req:Request) => req >> contentType

      when("it is executed")
      import Request.apply
      val headersWereModified = (HEAD, url) ~> {
        (x:ExecutedRequest) => x.fold(
          t => false,
          (status:Status, headers:Headers, body:Body, req:Request) => req.headers.contains(contentType)
        )
      }

      then("Status is returned")
      headersWereModified should be (true)
   }


  }

}