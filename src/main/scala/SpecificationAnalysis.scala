import java.io.{BufferedWriter, File, FileWriter, IOException, PrintWriter}
import java.util.{Date, Locale, Properties, TimeZone}
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.util

import Main.inputFileName
import com.sun.tools.javac.code.TypeTag
import edu.stanford.nlp._
import edu.stanford.nlp.simple._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.pipeline.{Annotation, CoreDocument, CoreSentence, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.trees.TreeCoreAnnotations

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.typeOf

object SpecificationAnalysis {
  var treeList: List[(Tree, List[Token])] = List()
  var treeList2: List[(Tree, List[CoreLabel])] = List()

  // 入力ファイルを解析する
  @throws[IOException]
  def analysis(str: String): Unit = {
    /** Core NLP */
    coreNlpParse(str)

    /** Simple */
    //simpleParse(str)
  }

  def coreNlpParse(str: String) = {
    /**
     * 解析したい要素をプロパティに設定
     * pos : 品詞
     * lemma : 原型
     * parse : 構文木
     * sentiment : 感情
     */
    val props = new Properties

    /**
     * Key : annotators
     * Value : tokenize, ssplit, pos, lemma, ner, parse, dcoref, sentiment
     */
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse")
    props.setProperty("tokenize.whitespace", "false")
    // パイプラインを設定する
    // new StanfordCoreNLP(props)で基本設定にできる
    val pipeline = new StanfordCoreNLP(props)
    // もっと複雑な設定にするには
    //   Properties props = new Properties();
    //   props.put("annotators", "tokenize, ssplit, pos, lemma, ner, depparse");
    //   props.put("ner.model", "edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz");
    //   props.put("ner.applyNumericClassifiers", "false");
    //   StanfordCoreNLP pipeline = new StanfordCoreNLP(props);

    // 入力ファイル
    val doc = new CoreDocument(str)
    //val annotation : Annotation = new Annotation(str)

    /** 解析する */
    pipeline.annotate(doc)
    /** 解析結果を出力する */
//    val tokens: List[CoreLabel] = doc.tokens().asScala.toList
//    for (tok <- tokens) {
//      System.out.println(String.format("%s\t%d\t%d", tok.word, tok.beginPosition, tok.endPosition))
//    }
    val sentences: List[CoreSentence] = doc.sentences().asScala.toList
    for(sent: CoreSentence <- sentences) {
      val tokens: List[CoreLabel] = sent.tokens().asScala.toList
      //val lemmas: List[String] = sent.lemmas().asScala.toList
      val parse: Tree = sent.constituencyParse()
      treeList2 :+= (parse, tokens)
    }
    // val tree = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).get(0).get(classOf[TreeCoreAnnotations.TreeAnnotation])
    //txtOut.println(tree)
    //pipeline.prettyPrint(annotation, out)
    //if (xmlOut != null) pipeline.xmlPrint(annotation, xmlOut)

  }

  def simpleParse(str: String) = {
    // parseのみ解析
    var doc: Document = new Document(str)

    //if (inputFileName != null) doc = new Document(IOUtils.slurpFileNoExceptions(inputFileName))
    //else doc = new Document("put your text to input.txt")

    //doc = new Document("Create a new end tag token, set its tag name to the empty string.")
    val sentences = doc.sentences().asScala.toList

    for(sent: Sentence <- sentences) {
      // System.out.println("The second word of the sentence '" + sent + "' is " + sent.word(1))
      val tokens: List[Token] = sent.tokens().asScala.toList
      //val lemmas: List[String] = sent.lemmas().asScala.toList
      val parse: Tree = sent.parse
      treeList :+= (parse, tokens)

      //val coref = sent.coref()
      //println(coref)
      import edu.stanford.nlp.coref.CorefCoreAnnotations
      import edu.stanford.nlp.ling.CoreAnnotations
      //      for (sentence <- coref) {
      //        System.out.println("---")
      //        System.out.println("mentions")
      //        sentence.get(classOf[CorefCoreAnnotations.CorefMentionsAnnotation])
      //        for (m <- sentence.get(classOf[CorefCoreAnnotations.CorefMentionsAnnotation])) {
      //          System.out.println("\t" + m)
      //        }
      //      }

      //txtOut.println(parse)
    }
  }
}