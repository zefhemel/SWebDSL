package swebdsl

import javax.servlet.http._

trait Forms {
  var formCounter = 0
  var inputCounter = 0
  var actionCounter = 0


  var request: HttpServletRequest
  protected var mode: PageMode.Value

  def write(s: String)

  def form(content: => Unit) {
    mode match {
      case PageMode.Render => {
        write("<form method=\"POST\">")
        write("<input type=\"hidden\" name=\"form-id\" value=\"" + formCounter + "\">")
        content
        write("</form>")
        formCounter += 1
      }
      case _ => content
    }
  }

  def input[T](a: T): T = {
    inputCounter += 1

    mode match {
      case PageMode.Render => {
        write("<input type=\"text\" name=\"input-" + inputCounter + "\" value=\"" + a + "\">")
        a
      }
      case PageMode.DataBind => {
        if (request.getParameter("input-" + inputCounter) != null) {
          val str = request.getParameter("input-" + inputCounter)
          strToT[T](a, str)
        } else {
          a
        }
      }
      case _ => a
    }
  }

  def inputPassword(a: String): String = {
    inputCounter += 1

    mode match {
      case PageMode.Render => {
        write("<input type=\"password\" name=\"input-" + inputCounter + "\" value=\"" + a + "\">")
        a
      }
      case PageMode.DataBind => {
        if (request.getParameter("input-" + inputCounter) != null) {
          request.getParameter("input-" + inputCounter)
        } else {
          a
        }
      }
      case _ => a
    }
  }

  def inputText(a: String): String = {
    inputCounter += 1

    mode match {
      case PageMode.Render => {
        write("<textarea name=\"input-" + inputCounter + "\" rows=\"8\" cols=\"60\">" + a + "</textarea>")
        a
      }
      case PageMode.DataBind => {
        if (request.getParameter("input-" + inputCounter) != null) {
          request.getParameter("input-" + inputCounter)
        } else {
          a
        }
      }
      case _ => a
    }
  }

  def inputHidden[T](a: T): T = {
    inputCounter += 1
    mode match {
      case PageMode.Render => {
        write("<input type=\"hidden\" name=\"input-" + inputCounter + "\" value=\"" + a + "\">")
        a
      }
      case PageMode.DataBind => {
        if (request.getParameter("input-" + inputCounter) != null) {
          val str = request.getParameter("input-" + inputCounter)
          strToT[T](a, str)
        } else {
          a
        }
      }
      case _ => a
    }
  }

  def strToT[T](a: T, str: String) =
    if (a.isInstanceOf[String])
      str.asInstanceOf[T]
    else if (a.isInstanceOf[Int])
      str.toInt.asInstanceOf[T]
    else if (a.isInstanceOf[Long])
      str.toLong.asInstanceOf[T]
    else
      throw new RuntimeException("Don't know how to create an input for " + a)

  def action(s: String)(c: => Unit) {
    actionCounter += 1
    mode match {
      case PageMode.Render => {
        write("<input type=\"submit\" name=\"action-" + actionCounter + "\" value=\"" + s + "\">")
      }
      case PageMode.Action => {
        if (request.getParameter("action-" + actionCounter) != null) {
          c
        }
      }
      case _ =>
    }
  }

}
