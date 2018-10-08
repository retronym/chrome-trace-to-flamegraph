package io.github.retronym.chrometraceflame

import java.lang
import java.nio.file.{Files, Path, Paths}

import com.fasterxml.jackson.core._
import com.google.common.collect
import com.google.common.collect.TreeRangeSet

import scala.collection.mutable

object ChromeTraceFlame {

  case class Record(var cat: String = "",
                    var name: String = "",
                    var id: Long = 0L,
                    var pid: String = null,
                    var ph: String = "",
                    var tid: String = null,
                    var ts: Long = 0L,
                    var cname: String = "",
                    var duration: Long = -1,
                    var argName: String = "",
                    var argValue: Long = 0L)

  class RecordStack {
    val data = new mutable.ArrayStack[Record]()
  }

  val asyncSlices = mutable.HashMap[Long, RecordStack]()
  val threadSlices = mutable.HashMap[String, RecordStack]()
  val counterEvents = mutable.HashMap[String, mutable.TreeMap[Long, Record]]()
  val gcPauses = TreeRangeSet.create[java.lang.Long]()

  def main(args: Array[String]): Unit = {
    val paths = args.toList.map(Paths.get(_))
    read(paths)
  }

  def read(files: List[Path]): Unit = {
    val factory = new JsonFactory()
    val outFileName = "/tmp/combined.trace"
    val writer = Files.newBufferedWriter(Paths.get(outFileName))
    val generator = factory.createGenerator(writer)

    def parse(gcOnly: Boolean): Unit = {
      for (file <- files) {
        val parser = factory.createParser(file.toFile)

        def acceptNext(expected: JsonToken): Unit = {
          val token = parser.nextToken
          require(token == expected, s"Expected $expected, found $token at ${parser.getCurrentLocation} in $file")
        }

        def acceptCurrent(expected: JsonToken): Unit = {
          val token = parser.currentToken()
          require(token == expected, s"Expected $expected, found $token at ${parser.getCurrentLocation}")
        }

        try {
          acceptNext(JsonToken.START_OBJECT)
          acceptNext(JsonToken.FIELD_NAME)
          require(parser.getCurrentName == "traceEvents", s"Expected `traceEvents`, found: ${parser.getCurrentName}")
          acceptNext(JsonToken.START_ARRAY)
          val next = parser.nextToken()
          while (parser.currentToken() != JsonToken.END_ARRAY) {
            acceptCurrent(JsonToken.START_OBJECT)
            var cat: String = ""
            var name: String = ""
            var id: Long = 0L
            var pid: String = null
            var ph: String = ""
            var tid: String = null
            var ts: Long = 0L
            var cname: String = ""
            var argName = ""
            var argValue = 0L
            var dur = 0L
            val next = parser.nextToken()
            while (parser.currentToken() != JsonToken.END_OBJECT) {
              acceptCurrent(JsonToken.FIELD_NAME)
              val fieldName = parser.getCurrentName

              fieldName match {
                case "cat" =>
                  acceptNext(JsonToken.VALUE_STRING)
                  cat = parser.getValueAsString
                case "name" =>
                  acceptNext(JsonToken.VALUE_STRING)
                  name = parser.getValueAsString
                case "id" =>
                  acceptNext(JsonToken.VALUE_NUMBER_INT)
                  id = parser.getLongValue()
                case "tid" =>
                  parser.nextToken //accept(JsonToken.VALUE_STRING)
                  tid = parser.getValueAsString()
                case "ph" =>
                  acceptNext(JsonToken.VALUE_STRING)
                  ph = parser.getValueAsString
                case "pid" =>
                  parser.nextToken //accept(JsonToken.VALUE_STRING)
                  pid = parser.getValueAsString
                case "ts" =>
                  acceptNext(JsonToken.VALUE_NUMBER_INT)
                  ts = parser.getLongValue
                case "dur" =>
                  acceptNext(JsonToken.VALUE_NUMBER_INT)
                  dur = parser.getLongValue
                case "cname" =>
                  acceptNext(JsonToken.VALUE_STRING)
                  cname = parser.getValueAsString
                case "args" =>
                  acceptNext(JsonToken.START_OBJECT)
                  acceptNext(JsonToken.FIELD_NAME)
                  argName = parser.getCurrentName
                  parser.nextToken()
                  argValue = parser.getValueAsLong()
                  acceptNext(JsonToken.END_OBJECT)
                case _ =>
                  sys.error(s"unsupported field name ${fieldName} at ${parser.getCurrentLocation}")
              }
              parser.nextToken()
            }
            acceptCurrent(JsonToken.END_OBJECT)
            parser.nextToken()
            if (gcOnly) {
              ph match {
                case "X" =>
                  if (tid == "GC") gcPauses.add(com.google.common.collect.Range.closed(ts, ts + dur))
                case _ =>
              }

            } else {
              ph match {
                case "B" =>
                  val recordStack = asyncSlices.getOrElseUpdate(id, new RecordStack)
                  val record = new Record
                  record.cat = cat
                  record.name = name
                  record.id = id
                  record.pid = pid
                  record.ph = ph
                  record.tid = tid
                  record.ts = ts
                  record.cname = cname
                  recordStack.data.push(record)
                case "E" =>
                  val recordStack = asyncSlices(id)
                  val record = recordStack.data.pop()
                  record.duration = ts - record.ts
                  def writeEvent(durationRange: collect.Range[lang.Long], rangeFudge: Long): Unit = {
                    generator.writeRaw("\n")
                    generator.writeStartObject()
                    generator.writeStringField("cat", record.cat)
                    generator.writeStringField("name", record.name)
                    generator.writeStringField("ph", "X")
                    generator.writeNumberField("id", record.id)
                    generator.writeStringField("tid", record.tid)
                    generator.writeStringField("pid", record.pid)
                    generator.writeNumberField("ts", durationRange.lowerEndpoint() + rangeFudge)
                    generator.writeNumberField("dur", durationRange.upperEndpoint() - durationRange.lowerEndpoint() - rangeFudge * 2)
                    if (record.cname != "")
                      generator.writeStringField("cname", record.cname)
                    generator.writeEndObject()
                  }
                  val durationRange: collect.Range[lang.Long] = com.google.common.collect.Range.closed[java.lang.Long](record.ts, record.ts + record.duration)
                  val gcIntersects = gcPauses.intersects(durationRange)
                  if (gcIntersects) {
                    val durationSet = TreeRangeSet.create[java.lang.Long]()
                    durationSet.add(durationRange)
                    durationSet.removeAll(gcPauses.subRangeSet(durationRange))
                    val iterator = durationSet.asRanges().iterator()
                    while (iterator.hasNext) {
                      val activeRange = iterator.next()
                      writeEvent(activeRange, rangeFudge = recordStack.data.size)
                    }
                  } else
                    writeEvent(durationRange, rangeFudge = 0L)
                case "C" =>
                  val map = counterEvents.getOrElseUpdate(pid, new mutable.TreeMap[Long, Record]())
                  val record = new Record
                  record.cat = cat
                  record.name = name
                  record.id = id
                  record.ph = ph
                  record.pid = pid
                  record.ts = ts
                  record.cname = cname
                  record.argName = argName
                  record.argValue = argValue
                  map(ts) = record
                case "X" =>
                  generator.writeStartObject()
                  generator.writeStringField("cat", cat)
                  generator.writeStringField("name", name)
                  generator.writeNumberField("id", id)
                  generator.writeStringField("ph", "X")
                  generator.writeStringField("tid", tid)
                  generator.writeStringField("pid", pid)
                  generator.writeNumberField("ts", ts)
                  if (cname != "")
                    generator.writeStringField("cname", cname)
                  generator.writeNumberField("dur", dur)
                  generator.writeEndObject()
                case _ =>
              }
              generator.writeRaw("\n")
            }
          }
          acceptNext(JsonToken.END_OBJECT)
        } catch {
          case ex: JsonParseException =>
            throw new JsonParseException(file.toString, ex.getLocation, ex)
        } finally {
          parser.close()
        }
      }
      if (!gcOnly) {
        for ((key, value) <- counterEvents) {
          for (record <- value.valuesIterator) {
            generator.writeStartObject()
            generator.writeStringField("cat", record.cat)
            generator.writeStringField("name", record.name)
            generator.writeNumberField("id", record.id)
            generator.writeStringField("ph", "C")
            generator.writeStringField("pid", record.pid)
            generator.writeNumberField("ts", record.ts)
            if (record.cname != "")
              generator.writeStringField("cname", record.cname)
            generator.writeFieldName("args")
            generator.writeStartObject()
            generator.writeFieldName(record.argName)
            generator.writeNumber(record.argValue)
            generator.writeEndObject()
            generator.writeEndObject()
          }
        }
        generator.writeEndArray()
        generator.writeEndObject()
      }
    }

    try {
      parse(gcOnly = true)

      generator.writeStartObject()
      generator.writeFieldName("traceEvents")
      generator.writeStartArray()
      parse(gcOnly = false)

    } finally {
      generator.close()
      writer.close()
    }
  }
}
