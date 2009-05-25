package swebdsl

import com.google.appengine.api.datastore._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait Cache {
  private var cache : HashMap[String, AnyRef] = new HashMap[String, AnyRef]
  
  def cache[T](key : String, l : => T) : T = {
    if(!cache.contains(key)) {
      cache(key) = l.asInstanceOf[AnyRef]
    }
    cache(key).asInstanceOf[T]    
  }
}

abstract class DataObject(key : String) {
  def singleton : DataObjectSingleton[_]
  var ent : Entity = null
  if(key != null) {
	ent = new Entity(singleton.kind, key)
  } else {
    ent = new Entity(singleton.kind)
  }
  def this() = this(null)
  
  def getValues() : HashMap[String, Any] = {
    var cls = this.getClass
    var map = new HashMap[String, Any]()
    for(m <- cls.getDeclaredMethods()) {
      var methodName = m.getName()
      if(methodName.endsWith("_$eq")) {
        var paramType = m.getParameterTypes()(0)
        if(paramType == classOf[Int] || paramType == classOf[Long] || paramType == classOf[Text] || paramType == classOf[String]) {
          var fieldName = methodName.substring(0, methodName.length-4)
	      var value = cls.getMethod(fieldName).invoke(this)
	      map(fieldName) = value
        }
      }
    }
    println(map)
    return map
  }
  
  def save() {
    var values = getValues()
    for(key <- values.keys) {
      ent.setProperty(key, values(key));
    }
    var ds = DatastoreServiceFactory.getDatastoreService()
    ds.put(ent)
  }
  
  def id : Long = ent.getKey().getId()
  
  def delete() {
    var ds = DatastoreServiceFactory.getDatastoreService()
    ds.delete(ent.getKey())
  }
  
  override def toString : String = {
    return id.toString()
  }
}


abstract class DataObjectSingleton[T <: DataObject](var cls : Class[_]) {
  def kind : String = cls.getSimpleName()

  def all() : ArrayBuffer[T] = {
    println("Performed all query for " + cls.getName())
    return queryToArrayBuffer(new Query(kind))
  }
  
  def where(property : String, value : Any) = {
    queryToArrayBuffer(new Query(kind).addFilter(property, Query.FilterOperator.EQUAL, value))
  }
  
  def apply() : T = {
    cls.newInstance().asInstanceOf[T]
  }
    
  def get(id : Long) : T = {
    var e = cls.newInstance().asInstanceOf[T]
    var ds = DatastoreServiceFactory.getDatastoreService()
    e.ent = ds.get(KeyFactory.createKey(cls.getSimpleName(), id))
    var values = e.ent.getProperties()
    var keys = values.keySet().iterator()
    while(keys.hasNext()) {
      var key = keys.next()
      cls.getMethod(key+"_$eq", cls.getMethod(key).getReturnType).invoke(e, values.get(key))
    }
    return e
  }
  
  private def queryToArrayBuffer(q : Query) : ArrayBuffer[T] = {
    var ds = DatastoreServiceFactory.getDatastoreService()
    var r = ds.prepare(q).asIterator()
    var results = new ArrayBuffer[T]()
    while(r.hasNext()) {
      var e = cls.newInstance().asInstanceOf[T]
      e.ent = r.next()
      var values = e.ent.getProperties()
      var keys = values.keySet().iterator()
      while(keys.hasNext()) {
        var key = keys.next()
        cls.getMethod(key+"_$eq", cls.getMethod(key).getReturnType).invoke(e, values.get(key))
      }
      results.append(e)
    }
    return results
  }
}

