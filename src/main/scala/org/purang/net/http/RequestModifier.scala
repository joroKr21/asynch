package org.purang.net.http

trait RequestModifier {
  def modify: Request => Request
}

object RequestModifier {

  def requestModifier(mod: Request => Request): RequestModifier = new RequestModifier {
    override def modify: (Request) => Request = mod
  }

}

