package swebdsl

import com.google.appengine.api.datastore.Text

object Predef {
  implicit def string2Text(s : String) = new Text(s)
  implicit def text2String(t : Text) = t.toString()
}