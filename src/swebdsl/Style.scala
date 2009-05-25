package swebdsl

class Style {
  import scala.collection.mutable.Stack
  
  val styles = new StringBuffer
  var styleStack = new Stack[String]
  var typeOfStyle = 'body
  //var parent : Style = null
  var css = new StringBuffer
  private var codeStyle : Symbol = 'body 

   
  def >> (s : Style) = s
 
  def header : Style = {
    styleStack.push(".header")
    this
  }

  def header(c : => Unit) : Style = {
    setSelector(".header", c)
    this
  }

  def section() = {
    styleStack.push(".section")
    this
  }
  
  def section(c : => Unit) : Style = {
    setSelector(".header", c)
    this
  }

  def navigate() = {
    styleStack.push(".navigate")
    this
  }
  
  def navigate(c : => Unit) : Style = {
    setSelector(".navigate", c)
    this
  }
  
  def block(s : String) = {
    styleStack.push("." + s)
    this
  }
  
  def body() = {
    styleStack.push("body")
    this
  }
  
  def body(c : => Unit) : Style = {
    setSelector("body", c)
    this
  }
    
  implicit def int2UnitInt(i : Int) = new UnitIntWrapper(i)
  //
  
  def setSelector(s : String, c : => Unit) = {
    styleStack.push(s)
    c
    styles.append(styleStack.mkString(" ") + " { " + css + " } \n")
    styleStack = new Stack[String]()
    css = new StringBuffer
  }
  
  def fontsize : UnitInt = throw new IllegalAccessError("Cannot read property, only set")
  def fontsize_=(i : UnitInt) {
    setSpecificStyle("font-size: " + i + "; ")
  }

  def color : String = throw new IllegalAccessError("Cannot read property, only set")
  def color_=(c : String) {
    setSpecificStyle("color: " + c + "; ")
  }

  def bgcolor : String = throw new IllegalAccessError("Cannot read property, only set")
  def bgcolor_=(c : String) {
    setSpecificStyle("background-color: " + c + "; ")
  }
  
  def fontfamily : String = throw new IllegalAccessError("Cannot read property, only set")
  def fontfamily_=(ff : String) {
    setSpecificStyle("font-family: " + ff + "; ")    
  }
  
  def width : UnitInt = throw new IllegalAccessError("Cannot read property, only set")
  def width_=(w : UnitInt) {
    setSpecificStyle("width: " + w + "; ")
  }

  def setSpecificStyle(css : String) {
    this.css.append(css)
    /*
    var s : Style = this
    while(s.parent != null) {
      s = s.parent
    }
    val key = s.styleStack map(_.name) mkString(" >> ")
    if(s.styles contains key) {
      s.styles(key).append(css)
    } else {
      s.styles(key) = new StringBuffer(css)
    }
    */
  }
  
  override def toString : String =
    "<style>" + styles.toString + "</style>"
  
}

abstract class UnitInt

class UnitIntWrapper(i : Int) {
  
  class PtUnitInt(i : Int) extends UnitInt {
    override def toString = i + "pt"
  }
  class EmUnitInt(i : Int) extends UnitInt {
    override def toString = i + "em"
  }
  class PercentUnitInt(i : Int) extends UnitInt {
    override def toString = i + "%"
  }
  def pt = new PtUnitInt(i)
  def em = new EmUnitInt(i)
  def percent = new PercentUnitInt(i)
}