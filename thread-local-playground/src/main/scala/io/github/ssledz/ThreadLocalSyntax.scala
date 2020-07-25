package io.github.ssledz

import java.lang.ref.WeakReference

object ThreadLocalSyntax {

  import io.github.ssledz.ThreadLocalSyntax.ThreadLocalOps.ThreadLocalMap

  implicit class ThreadLocalOps[T](val threadLocal: ThreadLocal[T]) extends AnyVal {

    private def getMap(t: Thread): ThreadLocalMap = new ThreadLocalMap(ThreadLocalOps.GetMap.invoke(threadLocal, t))

    def getT(t: Thread): Option[T] = getMap(t).getEntry(threadLocal).map(_.value[T])

    def removeT(t: Thread): Unit = getMap(t).remove(threadLocal)
  }

  private object ThreadLocalOps {
    private val GetMap = {
      val m = classOf[ThreadLocal[AnyRef]].getDeclaredMethod("getMap", classOf[Thread])
      m.setAccessible(true)
      m
    }

    private class ThreadLocalMap(val underlying: Object) extends AnyVal {
      def remove[T](t: ThreadLocal[T]): Unit = ThreadLocalMap.Remove.invoke(underlying, t)

      def getEntry[T](t: ThreadLocal[T]): Option[Entry] = Option(ThreadLocalMap.GetEntry.invoke(underlying, t)).map(e => new Entry(e))
    }

    private class Entry(val underlying: Object) extends AnyVal {

      def key[T]: ThreadLocal[T] = underlying.asInstanceOf[WeakReference[ThreadLocal[T]]].get()

      def value[T]: T = Entry.FieldValue.get(underlying).asInstanceOf[T]


    }

    object Entry {
      private val ThreadLocalEntryClass = Class.forName("java.lang.ThreadLocal$ThreadLocalMap$Entry")

      private val FieldValue = {
        val field = Entry.ThreadLocalEntryClass.getDeclaredField("value")
        field.setAccessible(true)
        field
      }
    }

    private object ThreadLocalMap {

      private val ThreadLocalMapClass = Class.forName("java.lang.ThreadLocal$ThreadLocalMap")


      private val Remove = {
        val m = ThreadLocalMapClass.getDeclaredMethod("remove", classOf[ThreadLocal[AnyRef]])
        m.setAccessible(true)
        m
      }

      private val GetEntry = {
        val m = ThreadLocalMapClass.getDeclaredMethod("getEntry", classOf[ThreadLocal[AnyRef]])
        m.setAccessible(true)
        m
      }

    }

  }

}
