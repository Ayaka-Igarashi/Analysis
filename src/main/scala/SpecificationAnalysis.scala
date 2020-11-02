import java.io.{BufferedWriter, File, FileWriter, IOException, PrintWriter}
import java.text.SimpleDateFormat
import java.util
import java.util.{ArrayList, Date, Locale, Properties, TimeZone}

import edu.stanford.nlp.simple._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.pipeline.{Annotation, Annotator, AnnotatorImplementations, CoreDocument, CoreSentence, ProtobufAnnotationSerializer, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.trees.TreeCoreAnnotations
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._

object SpecificationAnalysis {
  var treeList: List[(Tree, List[Token])] = List()
  var treeList2: List[(Tree, List[CoreLabel])] = List()

  var pipeline: StanfordCoreNLP = null

  // 入力ファイルを解析する
  @throws[IOException]
  def analysis(str: String): Unit = {
    /** Core NLP */
    //coreNlpParse(str)

    /** Simple */
    simpleParse(str)
  }

  @throws[IOException]
  def coreNlpParse(str: String) = {
    // パイプライン設定
    if (pipeline == null) {
      /**
       * 解析したい要素をプロパティに設定
       * pos : 品詞
       * lemma : 原型
       * parse : 構文木
       * sentiment : 感情
       */
      val props = new Properties()
      props.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse")
//      props.setProperty("language", "english")
//      props.setProperty("tokenize.class", "PTBTokenizer")
//      props.setProperty("tokenize.language", "en")
//      props.setProperty("parse.binaryTrees", "true")
//      props.setProperty("mention.type", "dep")
//      props.setProperty("coref.mode", "statistical")
//      props.setProperty("coref.md.type", "dep")

      /**
       * Key : annotators
       * Value : tokenize, ssplit, pos, lemma, ner, parse, dcoref, sentiment
       */
      //props.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse")
      //props.setProperty("tokenize.whitespace", "true") // 空白で区切られてないものは1つの文字として認識する
      //props.setProperty("tokenize.keepeol", "true")
      //props.setProperty("tokenize.options", "strictTreebank3=true")
      //props.setProperty("pos.model", "edu/stanford/nlp/models/pos-tagger/english-bidirectional-distsim.tagger")
      //props.setProperty("ssplit.boundaryTokenRegex", "\\\\.|[!?]+")
      //props.setProperty("tokenize.options", "splitHyphenated=false") // "-"で繋がる文字を1つの文字として認識する:false
      // パイプラインを設定する
      pipeline = new StanfordCoreNLP(props)
      // もっと複雑な設定にするには
      //   props.put("annotators", "tokenize, ssplit, pos, lemma, ner, depparse");
      //   props.put("ner.model", "edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz");
      //   props.put("ner.applyNumericClassifiers", "false");

//      val backend: AnnotatorImplementations = new AnnotatorImplementations()
//      val defaultTokenize: Annotator = backend.tokenizer(props)
//      val defaultSSplit: Annotator = backend.wordToSentences(props)
    }



    // 入力ファイル
    val doc = new CoreDocument(str)
    //val annotation : Annotation = new Annotation(str)


    /** 解析する */
    pipeline.annotate(doc)

//    defaultTokenize.annotate(annotation)
//    defaultSSplit.annotate(annotation)
//
//    val sentences: List[CoreMap] = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala.toList
//    //println(sentences)
//    for (sentence: CoreMap <- sentences) {
//      //val sent: Sentence = new Sentence(doc, sentence)
//      //println(sentence)
//      //val sent: Sentence = new Sentence(doc, new ProtobufAnnotationSerializer(false).toProtoBuilder(sentence), sentence.get(classOf[CoreAnnotations.TextAnnotation]), props)
//      //println(sent.tokens())
//    }


    /** 解析結果を出力する */
    val sentences: List[CoreSentence] = doc.sentences().asScala.toList
    for(sent: CoreSentence <- sentences) {
      val tokens: List[CoreLabel] = sent.tokens().asScala.toList
      val parse: Tree = sent.constituencyParse()
      treeList2 :+= (parse, tokens)
    }
    //pipeline.prettyPrint(annotation, out)
    //if (xmlOut != null) pipeline.xmlPrint(annotation, xmlOut)

  }

  def simpleParse(str: String) = {

//    val formatter = new SimpleDateFormat("mm:ss.SSS")
//    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
//    var start = System.currentTimeMillis
    // parseのみ解析
    val doc: Document = new Document(str)
    val sentences = doc.sentences().asScala.toList
    //var endtime = System.currentTimeMillis
    //System.out.println("時間1 = " + formatter.format(endtime - start))


    for(sent: Sentence <- sentences) {
      //start = System.currentTimeMillis
      // System.out.println("The second word of the sentence '" + sent + "' is " + sent.word(1))
      val tokens: List[Token] = sent.tokens().asScala.toList
      //val lemmas: List[String] = sent.lemmas().asScala.toList
//      start = System.currentTimeMillis
//      endtime = System.currentTimeMillis
      //System.out.println("時間2 = " + formatter.format(endtime - start))
      val parse: Tree = sent.parse
//      endtime = System.currentTimeMillis
      //System.out.println("時間3 = " + formatter.format(endtime - start))
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

    }

  }
}