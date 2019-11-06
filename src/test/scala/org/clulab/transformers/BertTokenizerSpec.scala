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
      val text = "yyy,!\u535A\u0000\u63A8zzz \u0001 \u00AD abc \u0000w\u0000x\u00ADy\u0005\u00AD 💩y"
      assert(tokenizer.tokenize(text) === Array(
        ((0, 3), "yyy"),
        ((3, 4), ","),
        ((4, 5), "!"),
        ((5, 6), "\u535A"),
        ((7, 8), "\u63A8"),
        ((8, 11), "zzz"),
        ((16, 19), "abc"),
        ((21, 26), "w\u0000x\u00ADy"),
        ((29, 32), "💩y")))
    }
  }
  "a cased BERT word-piece tokenizer" should {
    "produce the same word-pieces and encodings as in BERT's test_wordpiece_tokenizer and test_full_tokenizer" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.Cased)

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
        "e", "##e"), 0, WordPieceTokenizer.Cased)
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
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.Uncased)

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
    "produce the same word-pieces and encodings as in BERT for some ambiguous cases" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.Uncased)
      assert(tokenizer.tokenize("  W\u0302a\u0301nT\u00E9\u1E0E", (2, 10)) === Array(
        ((2, 8), "want"),
        ((8, 10), "ed")))
      assert(tokenizer.tokenize("\u00ADrun\u0000ni\u0001\u0002ng\u0005") === Array(
        ((1, 6), "runn"),
        ((6, 12), "ing")))
    }
  }
  "an uncased BERT tokenizer" should {
    "produce the same tokens and encodings as in BERT's test_full_tokenizer" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing", ",")
      val tokenizer = new BertTokenizer(new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.Uncased))

      assert(tokenizer.tokenize("UNwant\u00E9d,running") === Array(
        ((0, 2), "un"),
        ((2, 6), "want"),
        ((6, 8), "ed"),
        ((8, 9), ","),
        ((9, 13), "runn"),
        ((13, 16), "ing")))
      assert(tokenizer.encode("UNwant\u00E9d,running") === Array(
        ((0, 2), 7),
        ((2, 6), 4),
        ((6, 8), 5),
        ((8, 9), 10),
        ((9, 13), 8),
        ((13, 16), 9)))
      assert(tokenizer.tokenize("UNwant\u00E9d,runningX") === Array(
        ((0, 2), "un"),
        ((2, 6), "want"),
        ((6, 8), "ed"),
        ((8, 9), ","),
        ((9, 17), "runningx")))
      assert(tokenizer.encode("UNwant\u00E9d,runningX") === Array(
        ((0, 2), 7),
        ((2, 6), 4),
        ((6, 8), 5),
        ((8, 9), 10),
        ((9, 17), 0)))
    }
    "produce the same tokens as in BERT's test_is_whitespace, test_is_control, and test_is_punctuation" in {
      val vocab = Array("a", "b", "c", "##d", "cd", "-", "$", "`", ".", "[UNK]")
      val tokenizer = new BertTokenizer(new WordPieceTokenizer(vocab, 9, WordPieceTokenizer.Uncased))
      assert(tokenizer.tokenize(" \ta\rB\n\u00A0C\u0005d-💩$`.") === Array(
        ((2, 3), "a"),
        ((4, 5), "b"),
        ((7, 10), "cd"),
        ((10, 11), "-"),
        ((11, 13), "💩"),
        ((13, 14), "$"),
        ((14, 15), "`"),
        ((15, 16), ".")))
      assert(tokenizer.encode(" \ta\rB\n\u00A0C\u0005d-💩$`.") === Array(
        ((2, 3), 0),
        ((4, 5), 1),
        ((7, 10), 4),
        ((10, 11), 5),
        ((11, 13), 9),
        ((13, 14), 6),
        ((14, 15), 7),
        ((15, 16), 8)))
    }
  }
  "a cased BERT tokenizer" should {
    "produce the same tokens as BERT in various cases" in {
      val vocab = Array(
        "[UNK]", "M", "My", "c", "ca", "cat", "##t", "##at", "is", "luck", "lucky", "y", "the",
        "R", "RS", "##SP", "##PC", "##CA", "##A", "were", "weren", "n", "'", "t", "open", "at",
        "3", "##am", "last", "night", "!", "#", "f", "##umi", "##ng")
      val tokenizer = new BertTokenizer(new WordPieceTokenizer(vocab, 0, WordPieceTokenizer.Cased))
      val text = "My cat is lucky the RSPCA weren't open at 3am last night!!! #fuming 😡🐱"
      assert(tokenizer.tokenize(text) === Array(
        ((0, 2), "My"),
        ((3, 6), "cat"),
        ((7, 9), "is"),
        ((10, 15), "lucky"),
        ((16, 19), "the"),
        ((20, 22), "RS"),
        ((22,24), "PC"),
        ((24,25), "A"),
        ((26,31), "weren"),
        ((31,32), "'"),
        ((32,33), "t"),
        ((34,38), "open"),
        ((39,41), "at"),
        ((42,43), "3"),
        ((43,45), "am"),
        ((46,50), "last"),
        ((51,56), "night"),
        ((56,57), "!"),
        ((57,58), "!"),
        ((58,59), "!"),
        ((60,61), "#"),
        ((61,62), "f"),
        ((62,65), "umi"),
        ((65,67), "ng"),
        ((68,72), "😡🐱")))
    }
  }
}
