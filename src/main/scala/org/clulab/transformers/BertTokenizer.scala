package org.clulab.transformers

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer

trait Tokenizer {
  def tokenize(text: String, region: (Int, Int)): Array[(Int, Int)]
  def tokenize(text: String): Array[(Int, Int)] = tokenize(text, (0, text.length))
}

trait Encoder extends Tokenizer {
  def encode(text: String): Array[((Int, Int), Int)] = encode(text, (0, text.length))
  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)]
}

class BertTokenizer(vocab: Array[String], unkIndex: Int) extends Encoder {

  private val basicTokenizer = new BasicTokenizer
  private val wordPieceTokenizer = new WordPieceTokenizer(vocab, unkIndex)

  def tokenize(text: String, region: (Int, Int)): Array[(Int, Int)] = {
    basicTokenizer.tokenize(text, region).flatMap(wordPieceTokenizer.tokenize(text, _))
  }

  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)] = {
    basicTokenizer.tokenize(text, region).flatMap(wordPieceTokenizer.encode(text, _))
  }
}

class WordPieceTokenizer(vocab: Array[String], unkIndex: Int) extends Encoder {

  private val tokenToIndex = vocab.zipWithIndex.toMap
  private val (firsts, others) = vocab.sortBy{-_.length}.partition(!_.startsWith("##"))
  private val firstRegex = Pattern.compile(firsts.mkString("|"))
  private val otherRegex = Pattern.compile(others.map(_.substring(2)).mkString("|"))

  def tokenize(text: String, region: (Int, Int)): Array[(Int, Int)] = {
    val firstMatcher = firstRegex.matcher(text)
    firstMatcher.region(region._1, region._2)
    if (!firstMatcher.lookingAt()) {
      Array(region)
    } else {
      val buffer = ArrayBuffer((firstMatcher.start, firstMatcher.end))
      val otherMatcher = otherRegex.matcher(text)
      otherMatcher.region(firstMatcher.end, text.length)
      while (otherMatcher.lookingAt()) {
        buffer += otherMatcher.start -> otherMatcher.end
        otherMatcher.region(otherMatcher.end, text.length)
      }
      if (!otherMatcher.hitEnd) Array(region) else buffer.toArray
    }
  }

  def encode(text: String, region: (Int, Int)): Array[((Int, Int), Int)] = {
    for ((tokenRegion, i) <- tokenize(text, region).zipWithIndex) yield {
      val (start, end) = tokenRegion
      val tokenText = text.substring(start, end)
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

  def tokenize(text: String, region: (Int, Int)): Array[(Int, Int)] = {
    val buffer = ArrayBuffer.empty[(Int, Int)]
    val matcher = tokenRegex.matcher(text)
    matcher.region(region._1, region._2)
    while (matcher.find()) {
      if (!matcher.group().matches("""(?x)[\p{Cc}\p{Cf}]+  # all control characters""")) {
        buffer += matcher.start -> matcher.end
      }
    }
    buffer.toArray
  }
}
