package org.clulab.transformers

import java.text.Normalizer
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer

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
  val cased: String => String = identity
  val uncased: String => String = token => {
    Normalizer.normalize(token.toLowerCase, Normalizer.Form.NFD).replaceAll("""\p{Mn}""", "")
  }
}

class WordPieceTokenizer(vocab: Array[String], unkIndex: Int, textNormalizer: String => String) extends Encoder {

  private val tokenToIndex = vocab.zipWithIndex.toMap
  private val (firsts, others) = vocab.sortBy{-_.length}.partition(!_.startsWith("##"))
  private val firstRegex = Pattern.compile(firsts.mkString("|"))
  private val otherRegex = Pattern.compile(others.map(_.substring(2)).mkString("|"))

  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)] = {
    val (regionStart, regionEnd) = region
    // FIXME: this assumes normalizing the text does not change offsets, which is wrong
    val normalizedText = textNormalizer(text)
    val firstMatcher = firstRegex.matcher(normalizedText)
    firstMatcher.region(regionStart, regionEnd)
    val regions =
      if (!firstMatcher.lookingAt()) {
        Array(region)
      } else {
        val buffer = ArrayBuffer(firstMatcher.start -> firstMatcher.end)
        val otherMatcher = otherRegex.matcher(normalizedText)
        otherMatcher.region(firstMatcher.end, regionEnd)
        while (otherMatcher.lookingAt()) {
          buffer += (otherMatcher.start -> otherMatcher.end)
          otherMatcher.region(otherMatcher.end, regionEnd)
        }
        if (!otherMatcher.hitEnd) Array(region) else buffer.toArray
      }
    for (tokenRegion <- regions) yield (tokenRegion, normalizedText.substring(tokenRegion._1, tokenRegion._2))
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
      [^\p{IsWhite_Space}\p{IsHan}\p{P}$+<=>^`|~]+ # several non-whitespace characters (excluding the above)""")

  def tokenize(text: String, region: (Int, Int)): Array[((Int, Int), String)] = {
    val buffer = ArrayBuffer.empty[((Int, Int), String)]
    val matcher = tokenRegex.matcher(text)
    matcher.region(region._1, region._2)
    while (matcher.find()) {
      val group = matcher.group()
      if (!group.matches("""(?x)[\p{Cc}\p{Cf}]+  # all control characters""")) {
        buffer += matcher.start -> matcher.end -> group
      }
    }
    buffer.toArray
  }
}
