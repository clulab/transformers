package org.clulab.transformers

import java.text.Normalizer
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

trait Tokenizer {
  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)]
  def tokenize(text: String): Array[((Int, Int), String)] = tokenize(text, (0, text.length))
}

trait Encoder extends Tokenizer {
  def encode(text: String): Array[((Int, Int), Int)] = encode(text, (0, text.length))
  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)]
}

abstract class BertTokenizer(wordPieceTokenizer: WordPieceTokenizer) extends Encoder {

  private val basicTokenizer = new BasicTokenizer

  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)] = {
    basicTokenizer.tokenize(text, region).flatMap{ case (region, _) => wordPieceTokenizer.tokenize(text, region) }
  }

  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)] = {
    basicTokenizer.tokenize(text, region).flatMap{ case (region, _) => wordPieceTokenizer.encode(text, region) }
  }
}

object WordPieceTokenizer {
  trait TextNormalizer {
    def normalize(text: String): (String, Int => Int)
  }
  object Cased extends TextNormalizer {
    override def normalize(text: String): (String, Int => Int) = (text, identity)
  }
  object Uncased extends TextNormalizer {
    private def stripAccentsAndControlCharacters(text: String): String = {
      Normalizer.normalize(text, Normalizer.Form.NFD).replaceAll("""[\p{Mn}\p{Cc}\p{Cf}]""", "")
    }

    override def normalize(text: String): (String, Int => Int) = {
      val lowerCaseText = text.toLowerCase
      val normalizedText = stripAccentsAndControlCharacters(lowerCaseText)
      val offsetMap: Int => Int = if (lowerCaseText == normalizedText) identity else {
        var textIndex = 0
        val offsets = text.codePoints().iterator.asScala.flatMap{ codePoint =>
          val uniChar = new String(Character.toChars(codePoint))
          val length = stripAccentsAndControlCharacters(uniChar).length
          val offsets = IndexedSeq.fill(length)(textIndex)
          textIndex += uniChar.length
          offsets
        }
        offsets.toIndexedSeq :+ textIndex
      }
      (normalizedText, offsetMap)
    }
  }
}

class WordPieceTokenizer(vocab: Array[String], unkIndex: Int, textNormalizer: WordPieceTokenizer.TextNormalizer) extends Encoder {

  private val tokenToIndex = vocab.zipWithIndex.toMap
  private val (firsts, others) = vocab.sortBy{-_.length}.partition(!_.startsWith("##"))
  private val firstRegex = Pattern.compile(firsts.mkString("|"))
  private val otherRegex = Pattern.compile(others.map(_.substring(2)).mkString("|"))

  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)] = {
    val (regionStart, regionEnd) = region
    val (word, offsetMap) = textNormalizer.normalize(text.substring(regionStart, regionEnd))
    val firstMatcher = firstRegex.matcher(word)
    firstMatcher.region(0, word.length)
    val wordPieceRegions =
      if (!firstMatcher.lookingAt()) {
        Array(region)
      } else {
        val buffer = ArrayBuffer(firstMatcher.start -> firstMatcher.end)
        val otherMatcher = otherRegex.matcher(word)
        otherMatcher.region(firstMatcher.end, word.length)
        while (otherMatcher.lookingAt()) {
          buffer += (otherMatcher.start -> otherMatcher.end)
          otherMatcher.region(otherMatcher.end, word.length)
        }
        if (!otherMatcher.hitEnd) Array(region) else buffer.toArray
      }
    for ((pieceStart, pieceEnd) <- wordPieceRegions) yield {
      (regionStart + offsetMap(pieceStart), regionStart + offsetMap(pieceEnd)) -> word.substring(pieceStart, pieceEnd)
    }
  }

  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)] = {
    for (((tokenRegion, tokenText), i) <- tokenize(text, region).zipWithIndex) yield {
      val index = tokenToIndex.getOrElse(if (i == 0) tokenText else "##" + tokenText, unkIndex)
      (tokenRegion, index)
    }
  }
}

class BasicTokenizer extends Tokenizer {

  private val tokenRegex = Pattern.compile("""(?x)
      \p{IsHan}|                                   # a single Chinese character, or
      [\p{P}$+<=>^`|~]|                            # a single punctuation character, or
      (?![\p{Cc}\p{Cf}])                           # not starting with a control character
      [^\p{IsWhite_Space}\p{IsHan}\p{P}$+<=>^`|~]+ # several non-whitespace characters (excluding the above)
      (?<![\p{Cc}\p{Cf}])                          # not ending with a control character
  """)

  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)] = {
    val buffer = ArrayBuffer.empty[((Int, Int), String)]
    val matcher = tokenRegex.matcher(text)
    matcher.region(region._1, region._2)
    while (matcher.find()) {
      buffer += matcher.start -> matcher.end -> matcher.group()
    }
    buffer.toArray
  }
}
