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
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.trees.TreeCoreAnnotations

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.typeOf

object SpecificationAnalysis {
  var treeList: List[(Tree, List[Token])] = List()

  // 入力ファイルを解析する
  @throws[IOException]
  def analysis(str: String): Unit = {
    /** Core NLP */

    /*
    // 設定時間
    var start = System.currentTimeMillis
    val formatter = new SimpleDateFormat("mm:ss.SSS")
    formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
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
    // パイプラインを設定する
    // new StanfordCoreNLP(props)で基本設定にできる
    val pipeline = new StanfordCoreNLP(props)
    // もっと複雑な設定にするには
    //   Properties props = new Properties();
    //   props.put("annotators", "tokenize, ssplit, pos, lemma, ner, depparse");
    //   props.put("ner.model", "edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz");
    //   props.put("ner.applyNumericClassifiers", "false");
    //   StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
    var endtime = System.currentTimeMillis
    System.out.println("設定時間 = " + formatter.format(endtime - start))

    // 読み込み時間
    start = System.currentTimeMillis
    // 入力ファイル設定
    var annotation : Annotation = null
    if (inputFileName != null) annotation = new Annotation(IOUtils.slurpFileNoExceptions(inputFileName))
    else annotation = new Annotation("Karma of humans is AI. She was sad.") // She sent an email to Tokyo Tech University yestuday.
    // 読み込み終了時間
    endtime = System.currentTimeMillis
    System.out.println("読み込み時間 = " + formatter.format(endtime - start))

    // 解析スタート時間記録
    start = System.currentTimeMillis
    /** run all the selected Annotators on this text解析する */
    pipeline.annotate(annotation)
    // 解析時間記録
    endtime = System.currentTimeMillis
    System.out.println("解析時間 = " + formatter.format(endtime - start))

    // 出力スタート時間記録
    start = System.currentTimeMillis
    /** 解析結果を出力する */

    val tree = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).get(0).get(classOf[TreeCoreAnnotations.TreeAnnotation])
    txtOut.println(tree)
    //pipeline.prettyPrint(annotation, out)
    //if (xmlOut != null) pipeline.xmlPrint(annotation, xmlOut)

    // 解析出力時間記録
    endtime = System.currentTimeMillis
    System.out.println("解析出力時間 = " + formatter.format(endtime - start))

    */////

    /** Simple */

    // parseのみ解析
    var doc: Document = null
    /*
    if (inputFileName != null) doc = new Document(IOUtils.slurpFileNoExceptions(inputFileName))
    else doc = new Document("put your text to input.txt")
    */
    doc = new Document(str)
    val sentences = doc.sentences().asScala.toList

    for(sent: Sentence <- sentences) {
      // System.out.println("The second word of the sentence '" + sent + "' is " + sent.word(1))
      val tokens: List[Token] = sent.tokens().asScala.toList
      //val lemmas: List[String] = sent.lemmas().asScala.toList
      val parse: Tree = sent.parse
      treeList :+= (parse, tokens)
      //txtOut.println(parse)
    }
  }
}