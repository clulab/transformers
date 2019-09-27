package org.clulab.transformers

import org.scalatest.{Matchers, WordSpec}

class BertTokenizerSpec  extends WordSpec with Matchers {
  "a BERT basic tokenizer" should {
    val tokenizer = new BasicTokenizer()
    "produce the same token boundaries as in BERT's test_full_tokenizer" in {
      val text = "UNwant\u00E9d,running"
      assert(tokenizer.tokenize(text) === Array(
        ((0, 8), "UNwant\u00E9d"),
        ((8, 9), ","),
        ((9, 16), "running")))
    }
    "produce the same token boundaries as in BERT's test_chinese" in {
      val text = "ah\u535A\u63A8zz"
      assert(tokenizer.tokenize(text) === Array(
        ((0, 2), "ah"),
        ((2, 3), "\u535A"),
        ((3, 4), "\u63A8"),
        ((4, 6), "zz")))
    }
    "produce the same token boundaries as in BERT's test_basic_tokenizer_no_lower" in {
      val text = " \tHeLLo!how  \n Are yoU?  "
      assert(tokenizer.tokenize(text) === Array(
        ((2, 7), "HeLLo"),
        ((7, 8), "!"),
        ((8, 11), "how"),
        ((15, 18), "Are"),
        ((19, 22), "yoU"),
        ((22, 23), "?")))
    }
    "produce the same token boundaries as BERT for some complex cases" in {
      val text = "yyy,!\u535A\u0000\u63A8zzz \u0001 \u00AD abc \u0000w\u0000x\u00ADy\u00AD"
      assert(tokenizer.tokenize(text) === Array(
        ((0, 3), "yyy"),
        ((3, 4), ","),
        ((4, 5), "!"),
        ((5, 6), "\u535A"),
        ((7, 8), "\u63A8"),
        ((8, 11), "zzz"),
        ((16, 19), "abc"),
        ((20, 27), "\u0000w\u0000x\u00ADy\u00AD")))
    }
  }
  "a cased BERT word-piece tokenizer" should {
    "produce the same word-pieces and encodings as in BERT's test_wordpiece_tokenizer and test_full_tokenizer" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.cased)

      assert(tokenizer.tokenize("unwanted") === Array(
        ((0, 2), "un"),
        ((2, 6), "want"),
        ((6, 8), "ed")))
      assert(tokenizer.tokenize(",") === Array(
        ((0, 1), ",")))
      assert(tokenizer.tokenize("running") === Array(
        ((0, 4), "runn"),
        ((4, 7), "ing")))
      assert(tokenizer.tokenize("unwantedX") === Array(
        ((0, 9), "unwantedX")))
      assert(tokenizer.tokenize("UNwant\u00E9d") === Array(
        ((0, 8), "UNwant\u00E9d")))

      assert(tokenizer.encode("unwanted") === Array(
        ((0, 2), 7),
        ((2, 6), 4),
        ((6, 8), 5)))
      assert(tokenizer.encode(",") === Array(
        ((0, 1), 10)))
      assert(tokenizer.encode("running") === Array(
        ((0, 4), 8),
        ((4, 7), 9)))
      assert(tokenizer.encode("unwantedX") === Array(
        ((0, 9), 0)))
      assert(tokenizer.encode("UNwant\u00E9d") === Array(
        ((0, 8), 0)))
    }
    "produce the same word-pieces as in BERT for some ambiguous cases" in {
      val tokenizer = new WordPieceTokenizer(Array(
        "e", "##e", "es", "##es", "est", "##est",
        "s", "##s", "##st",
        "t", "##t", "##ti", "time", "##time",
        "i", "##i", "im", "##im", "##ime",
        "m", "##m", "me", "##me",
        "e", "##e"), 0, WordPieceTokenizer.cased)
      assert(tokenizer.tokenize("estime") === Array(
        ((0, 3), "est"),
        ((3, 6), "ime")))
      assert(tokenizer.tokenize("stime") === Array(
        ((0, 1), "s"),
        ((1, 5), "time")))
      assert(tokenizer.tokenize("stim") === Array(
        ((0, 1), "s"),
        ((1, 3), "ti"),
        ((3, 4), "m")))
    }
  }
  "an uncased BERT word-piece tokenizer" should {
    "produce the same word-pieces and encodings as in BERT's test_wordpiece_tokenizer and test_full_tokenizer" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.uncased)

      assert(tokenizer.tokenize("UNwant\u00E9d") === Array(
        ((0, 2), "un"),
        ((2, 6), "want"),
        ((6, 8), "ed")))
      assert(tokenizer.tokenize(",") === Array(
        ((0, 1), ",")))
      assert(tokenizer.tokenize("running") === Array(
        ((0, 4), "runn"),
        ((4, 7), "ing")))
      assert(tokenizer.tokenize("unwantedX") === Array(
        ((0, 9), "unwantedx")))

      assert(tokenizer.encode("UNwant\u00E9d") === Array(
        ((0, 2), 7),
        ((2, 6), 4),
        ((6, 8), 5)))
      assert(tokenizer.encode(",") === Array(
        ((0, 1), 10)))
      assert(tokenizer.encode("running") === Array(
        ((0, 4), 8),
        ((4, 7), 9)))
      assert(tokenizer.encode("unwantedX") === Array(
        ((0, 9), 0)))
    }
    "produce the same word-pieces and encodings as in BERT for some ambiguous cases" ignore {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.uncased)
      assert(tokenizer.tokenize("UNwante\u0301d") === Array(
        ((0, 2), "un"),
        ((2, 4), "want"),
        ((4, 7), "ed")))
    }
  }

  private def assertTokenCodes(encoder: Encoder, text: String, codes: Array[Int]): Unit = {
    assert(encoder.encode(text).map{ case (_, code) => code } === codes)
  }
}
