package com.gu.contentapi.json

import com.gu.contentapi.client.model.v1._
import com.twitter.scrooge.ThriftEnum
import io.circe._
import io.circe.syntax._
import com.gu.contentapi.circe.CirceScroogeMacros._
import com.gu.contentatom.thrift.{Atom, AtomType, ContentChangeDetails}
import io.circe.generic.auto._

object CirceDeserialization {

  private val LowerCaseFollowedByUpperCase = """([a-z])([A-Z])""".r

  /**
    * Convert a PascalCase string to a lowercase hyphenated string
    */
  private def pascalCaseToHyphenated(s: String): String =
    LowerCaseFollowedByUpperCase.replaceAllIn(s, m => m.group(1) + "-" + m.group(2)).toLowerCase

  implicit def thriftEnumEncoder[T <: ThriftEnum]: Encoder[T] = Encoder[String].contramap(t => pascalCaseToHyphenated(t.name))

  implicit val networkFrontEncoder: Encoder[NetworkFront] = encodeThriftStruct[NetworkFront]

  implicit val contentFieldsEncoder = Encoder[ContentFields]
  implicit val editionEncoder = Encoder[Edition]
  implicit val sponsorshipEncoder = Encoder[Sponsorship]
  implicit val tagEncoder = Encoder[Tag]
  implicit val assetEncoder = Encoder[Asset]
  implicit val elementEncoder = Encoder[Element]
  implicit val referenceEncoder = Encoder[Reference]
  implicit val blockEncoder = Encoder[Block]
  implicit val blocksEncoder = genBlocksEncoder
  implicit val rightsEncoder = Encoder[Rights]
  implicit val crosswordEntryEncoder = genCrosswordEntryEncoder
  implicit val crosswordEncoder = Encoder[Crossword]
  implicit val contentStatsEncoder = Encoder[ContentStats]
  implicit val sectionEncoder = Encoder[Section]
  implicit val debugEncoder = Encoder[Debug]
  implicit val atomsEncoder = genAtomsEncoder//Encoder[Atoms]
  implicit val atomEncoder = genAtomEncoder//Encoder[Atom]
  implicit val contentEncoder = Encoder[Content]
  implicit val mostViewedVideoEncoder = Encoder[MostViewedVideo]
  implicit val packageEncoder = Encoder[Package]

  implicit val blockMapEncoder = Encoder.instance[scala.collection.Map[String,Seq[Block]]] { m =>
    val fields = m.toList.map {
      case (k, v) => k -> Json.fromValues(v.map(_.asJson))
    }
    Json.fromFields(fields)
  }

  implicit val separatorLocationsEncoder = Encoder.instance[scala.collection.Map[String,Seq[Int]]] { s =>
    val fields = s.toList.map {
      case (k, v) => k -> Json.fromValues(v.map(_.asJson))
    }
    Json.fromFields(fields)
  }

  def genBlocksEncoder: Encoder[Blocks] = Encoder.instance[Blocks] { blocks =>
    val fields = List(
      blocks.main.map("main" -> _.asJson),
      blocks.body.map("body" -> _.asJson),
      blocks.totalBodyBlocks.map("totalBodyBlocks" -> _.asJson),
      blocks.requestedBodyBlocks.map("requestedBodyBlocks" -> _.asJson)
    ).flatten

    Json.fromFields(fields)
  }

  def genCrosswordEntryEncoder: Encoder[CrosswordEntry] = Encoder.instance[CrosswordEntry] { crossword =>
    val fields = List(
      Some("id" -> crossword.id.asJson),
      crossword.number.map("number" -> _.asJson),
      crossword.humanNumber.map("humanNumber" -> _.asJson),
      crossword.direction.map("direction" -> _.asJson),
      crossword.position.map("position" -> _.asJson),
      crossword.separatorLocations.map("separatorLocations" -> _.asJson),
      crossword.length.map("length" -> _.asJson),
      crossword.clue.map("clue" -> _.asJson),
      crossword.group.map("group" -> _.asJson),
      crossword.solution.map("solution" -> _.asJson),
      crossword.format.map("format" -> _.asJson)
    ).flatten

    Json.fromFields(fields)
  }

  def genAtomsEncoder: Encoder[Atoms] = Encoder.instance[Atoms] { atoms =>
    val fields = List(
      atoms.quizzes.map("quizzes" -> _.asJson),
      atoms.media.map("media" -> _.asJson),
      atoms.explainers.map("explainers" -> _.asJson)
    ).flatten

    Json.fromFields(fields)
  }

  def genAtomEncoder: Encoder[Atom] = Encoder.instance[Atom] { atom =>
    val fields = List(
      Some("id" -> atom.id.asJson),
      Some("atomType" -> atom.atomType.asJson),
      Some("labels" -> atom.labels.asJson),
      Some("defaultHtml" -> atom.defaultHtml.asJson),
      Some("change" -> atom.contentChangeDetails.asJson)
    ).flatten

    Json.fromFields(fields)
  }

}
