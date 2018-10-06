package io.github.retronym.chrometraceflame

import java.nio.file.{Files, Paths}

import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.util.{DefaultPrettyPrinter, MinimalPrettyPrinter}
import org.yaml.snakeyaml.util.ArrayStack

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ChromeTraceFlame {

  case class Record(var cat: String = "",
                    var name: String = "",
                    var id: Long = 0L,
                    var pid: String = null,
                    var ph: String = "",
                    var tid: String = null,
                    var ts: Long = 0L,
                    var cname: String = "",
                    var duration: Long = -1
                   )
  class RecordStack {
    val data = new mutable.ArrayStack[Record]()
    var depth: Int = 0
  }
  val asyncSlices = mutable.HashMap[Long, RecordStack]()
  val threadSlices = mutable.HashMap[String, RecordStack]()

  def main(args: Array[String]): Unit = {
    val fileName = if (args.size == 0) "/tmp/combined.trace" else args(0)
    val factory = new JsonFactory()
    val parser = factory.createParser(Paths.get(fileName).toFile)
    val outFileName = fileName.replaceAll(".trace", "") + ".processed.trace"
    val writer = Files.newBufferedWriter(Paths.get(outFileName))
    val generator = factory.createGenerator(writer)

    def acceptNext(expected: JsonToken): Unit = {
      val token = parser.nextToken
      require(token == expected, s"Expected $expected, found $token at ${parser.getCurrentLocation}")
    }
    def acceptCurrent(expected: JsonToken): Unit = {
      val token = parser.currentToken()
      require(token == expected, s"Expected $expected, found $token at ${parser.getCurrentLocation}")
    }

    try {
      acceptNext(JsonToken.START_OBJECT)
      generator.writeStartObject()
      acceptNext(JsonToken.FIELD_NAME)
      require(parser.getCurrentName == "traceEvents", s"Expected `traceEvents`, found: ${parser.getCurrentName}")
      generator.writeFieldName(parser.getCurrentName)
      acceptNext(JsonToken.START_ARRAY)
      generator.writeStartArray()
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
        ph match {
          case "b" =>
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
          case "e" =>
            val recordStack = asyncSlices(id)
            val record = recordStack.data.pop()
            record.duration = ts - record.ts
            generator.writeRaw("\n")
            generator.writeStartObject()
            generator.writeStringField("cat", record.cat)
            generator.writeStringField("name", record.name)
            generator.writeStringField("ph", "X")
            generator.writeNumberField("id", record.id)
            generator.writeStringField("tid", record.id.toString) // Intentional
            generator.writeStringField("pid", record.pid)
            generator.writeNumberField("ts", record.ts)
            generator.writeNumberField("dur", record.duration)
            if (record.cname != "")
              generator.writeStringField("cname", record.cname)
            generator.writeEndObject()
          case "C" =>
            generator.writeStartObject()
            generator.writeStringField("cat", cat)
            generator.writeStringField("name", name)
            generator.writeNumberField("id", id)
            generator.writeStringField("ph", "C")
            generator.writeStringField("tid", tid)
            generator.writeStringField("pid", pid)
            generator.writeNumberField("ts", ts)
            // generator.writeNumberField("dur", dur)
            if (cname != "")
              generator.writeStringField("cname", cname)
            generator.writeFieldName("args")
            generator.writeStartObject()
            generator.writeFieldName(argName)
            generator.writeNumber(argValue)
            generator.writeEndObject()
            generator.writeEndObject()
          case "X" =>
            generator.writeStartObject()
            generator.writeStringField("cat", cat)
            generator.writeStringField("name", name)
            generator.writeNumberField("id", id)
            generator.writeStringField("ph", "X")
            generator.writeStringField("tid", tid)
            generator.writeStringField("pid", pid)
            generator.writeNumberField("ts", ts)
            // generator.writeNumberField("dur", dur)
            if (cname != "")
              generator.writeStringField("cname", cname)
            generator.writeNumberField("dur", dur)
            generator.writeEndObject()
          case _ =>
        }

      }
      generator.writeEndArray()
      acceptNext(JsonToken.END_OBJECT)
      generator.writeEndObject()
    } finally {
      parser.close()
      generator.close()
      writer.close()
    }
  }
}
