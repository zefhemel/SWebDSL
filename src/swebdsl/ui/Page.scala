package swebdsl.ui

import collection.mutable.HashMap
import java.io._
import javax.servlet.http._

object PageMode extends Enumeration {
  type PageMode = Value
  val DataBind, Action, Render = Value
}

case class IgnoreMe()

trait Cache {
  private var cache: HashMap[String, AnyRef] = new HashMap[String, AnyRef]

  def cache[T](key: String, l: => T): T = {
    if (!cache.contains(key)) {
      cache(key) = l.asInstanceOf[AnyRef]
    }
    cache(key).asInstanceOf[T]
  }
}


abstract case class Page(ign: IgnoreMe) extends Forms with Cache {
  var sectionDepth = 1
  var style = new Style
  var out: PrintWriter = null
  var request: HttpServletRequest = null
  var response: HttpServletResponse = null
  protected var mode = PageMode.DataBind

  def ui

  def title: String = productPrefix

  def name: String = productPrefix.toLowerCase

  def init(out: PrintWriter, req: HttpServletRequest, res: HttpServletResponse) {
    this.out = out
    this.request = req
    this.response = res
    postInit
  }

  def postInit {

  }

  def resetCounters {
    actionCounter = 0
    formCounter = 0
    inputCounter = 0
  }

  def write(s: String) {
    out.print(s)
  }

  def doDatabind {
    mode = PageMode.DataBind
    ui
  }

  def doAction {
    mode = PageMode.Action
    ui
  }

  def doRender {
    mode = PageMode.Render
    write("<html><head>")
    write(style.toString)
    write("<title>" + title + "</title>")
    write("</head><body>")
    ui
    write("</body></html>")
  }

  def html(o: AnyRef) {
    mode match {
      case PageMode.Render => write(o.toString)
      case _ =>
    }
  }


  def header(content: => Unit) = renderSimple("<h" + sectionDepth + " class=\"header\">", "</h" + sectionDepth + ">", content)

  def header(s: String) = renderSimple("<h" + sectionDepth + " class=\"header\">", "</h" + sectionDepth + ">", text(s))

  def text(s: String) = html(s) // @TODO: escape this
  def block(s: String)(content: => Unit) = renderSimple("<div class=\"" + s + "\">", "</div>", content)

  def img(s: String) = html("<img src=\"" + s + "\" class=\"img\"/>")

  def br = html("<br/>")

  def hr = html("<hr/>")

  def navigate(url: String)(content: => Unit) = renderSimple("<a href=\"" + url + "\" class=\"navigate\">", "</a>", content)

  def navigate(p: Page)(content: => Unit) = renderSimple("<a href=\"" + buildPageUrl(p) + "\" class=\"navigate\">", "</a>", content)

  def buildPageUrl(p: Page): String = {
    val c = p.getClass
    var queryStr = new StringBuilder
    for (i <- 0 until p.productArity) {
      val value = p.productElement(i)
      queryStr.append("/" + value.toString)
    }
    request.getContextPath + "/" + p.name + queryStr.toString
  }

  def section(content: => Unit) {
    mode match {
      case PageMode.Render => {
        write("<div class=\"section\">")
        sectionDepth += 1
        content
        sectionDepth -= 1
        write("</div>")
      }
      case _ => content
    }
  }

  def list(content: => Unit) = renderSimple("<ul class=\"list\">", "</ul>", content)

  def listitem(s: String) = renderSimple("<li class=\"listitem\">", "</li>", text(s))

  def listitem(content: => Unit) = renderSimple("<li class=\"listitem\">", "</li>", content)

  def table(content: => Unit) = renderSimple("<table>", "</table>", content)

  def row(content: => Unit) = renderSimple("<tr>", "</tr>", content)

  def col(content: => Unit) = renderSimple("<td>", "</td>", content)

  protected def renderSimple(beginTag: String, endTag: String, content: => Unit) = {
    mode match {
      case PageMode.Render => {
        write(beginTag)
        content
        write(endTag)
      }
      case _ => content
    }
  }

  protected def html(html: String) {
    mode match {
      case PageMode.Render => {
        write(html)
      }
      case _ =>
    }
  }

  def goto(url: String) {
    response.sendRedirect(url)
  }

  def goto(p: Page) {
    response.sendRedirect(buildPageUrl(p))
  }

  def this() = this (new IgnoreMe)

  if (productArity == 1 && (productElement(0) == IgnoreMe()))
    throw new IllegalStateException("Pages must be defined as case classes")
}