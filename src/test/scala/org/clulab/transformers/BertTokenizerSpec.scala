package org.clulab.transformers

import org.scalatest.{Matchers, WordSpec}

class BertTokenizerSpec  extends WordSpec with Matchers {
  "a BERT basic tokenizer" should {
    val tokenizer = new BasicTokenizer()
    "produce the same token boundaries as in BERT's test_full_tokenizer" in {
      val text = "UNwant\u00E9d,running"
      assertTokenStrings(tokenizer, text, Array("UNwant\u00E9d", ",", "running"))
    }
    "produce the same token boundaries as in BERT's test_chinese" in {
      val text = "ah\u535A\u63A8zz"
      assertTokenStrings(tokenizer, text, Array("ah", "\u535A", "\u63A8", "zz"))
    }
    "produce the same token boundaries as in BERT's test_basic_tokenizer_no_lower" in {
      val text = " \tHeLLo!how  \n Are yoU?  "
      assertTokenStrings(tokenizer, text, Array("HeLLo", "!", "how", "Are", "yoU", "?"))
    }
    "produce the same token boundaries as BERT for some complex cases" in {
      val text = "yyy,!\u535A\u0000\u63A8zzz \u0001 \u00AD abc \u0000w\u0000x\u00ADy\u00AD"
      assertTokenStrings(
        tokenizer, text, Array("yyy", ",", "!", "\u535A", "\u63A8", "zzz", "abc", "\u0000w\u0000x\u00ADy\u00AD"))
    }
  }
  "a BERT word-piece tokenizer" should {
    "produce the same word-pieces as in BERT's test_wordpiece_tokenizer" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing")
      val tokenizer = new WordPieceTokenizer(vocab, 0)
      assertTokenStrings(tokenizer, "unwanted", Array("un", "want", "ed"))
      assertTokenStrings(tokenizer, "running", Array("runn", "ing"))
      assertTokenStrings(tokenizer, "unwantedX", Array("unwantedX"))
    }
    "produce the same encodings as XXXX" in {
      val vocab = Array("[UNK]", "[CLS]", "[SEP]", "want", "##want", "##ed", "wa", "un", "runn", "##ing")
      val tokenizer = new BertTokenizer(vocab, 0)
      assertTokenCodes(tokenizer, "unwanted", Array(7, 4, 5))

    }
    "produce the same word-pieces as in BERT for some ambiguous cases" in {
      val tokenizer = new WordPieceTokenizer(Array(
        "e", "##e", "es", "##es", "est", "##est",
        "s", "##s", "##st",
        "t", "##t", "##ti", "time", "##time",
        "i", "##i", "im", "##im", "##ime",
        "m", "##m", "me", "##me",
        "e", "##e"), 0)
      assertTokenStrings(tokenizer, "estime", Array("est", "ime"))
      assertTokenStrings(tokenizer, "stime", Array("s", "time"))
      assertTokenStrings(tokenizer, "stim", Array("s", "ti", "m"))
    }
  }

  private def assertTokenStrings(tokenizer: Tokenizer, text: String, expected: Array[String]): Unit = {
    assert(tokenizer.tokenize(text).map{ case (start, end) => text.substring(start, end) } === expected)
  }

  private def assertTokenCodes(encoder: Encoder, text: String, codes: Array[Int]): Unit = {
    assert(encoder.encode(text).map{ case (_, code) => code } === codes)
  }
}
