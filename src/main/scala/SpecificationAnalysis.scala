import java.io.{BufferedWriter, File, FileWriter, IOException, PrintWriter}
import java.util.{Date, Locale, Properties, TimeZone}
import java.text.DateFormat._
import java.text.SimpleDateFormat
import java.util

import com.sun.tools.javac.code.TypeTag
import edu.stanford.nlp._
import edu.stanford.nlp.simple._
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree

import scala.collection.JavaConverters._

import scala.reflect.runtime.universe.typeOf

object SpecificationAnalysis {
  var treeList: List[Tree] = List()

  @throws[IOException]
  def analysis(args: Array[String]): Unit = {
    // 出力ファイル設定
    var out : PrintWriter= null
    if (args.length > 1) {
      out = new PrintWriter(new BufferedWriter(new FileWriter(new File(args(1)))))
      System.out.println("input -> output.txt")
    }
    else out = new PrintWriter(System.out)
    var xmlOut : PrintWriter = null
    if (args.length > 2) {
      xmlOut = new PrintWriter(args(2))
      System.out.println("input -> output.xml")
    }

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
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref, sentiment")
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
    if (args.length > 0) annotation = new Annotation(IOUtils.slurpFileNoExceptions(args(0)))
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
    pipeline.prettyPrint(annotation, out)
    if (xmlOut != null) pipeline.xmlPrint(annotation, xmlOut)

    // 解析出力時間記録
    endtime = System.currentTimeMillis
    System.out.println("解析出力時間 = " + formatter.format(endtime - start))

    */

    // parseのみ解析
    var doc: Document = null
    if (args.length > 0) doc = new Document(IOUtils.slurpFileNoExceptions(args(0)))
    else doc = new Document("add your text here! It can contain multiple sentences.") // She sent an email to Tokyo Tech University yestuday.

    val sentences = doc.sentences().asScala.toList
    /*
    val sentences_java: util.List[Sentence] = doc.sentences()
    var sentences: List[Sentence] = List()
    for (i <- 0 to sentences_java.size() - 1) {
      sentences :+= sentences_java.get(i)
    }*/
    //System.out.println(">1")
    for(sent: Sentence <- sentences) {
      // System.out.println("The second word of the sentence '" + sent + "' is " + sent.word(1))
      // System.out.println("The third lemma of the sentence '" + sent + "' is " + sent.lemma(2))
      val parse: Tree = sent.parse
      treeList :+= parse
      out.println(parse)
      //System.out.println(">2")
    }

    // ファイルを閉じる
    IOUtils.closeIgnoringExceptions(out)
    IOUtils.closeIgnoringExceptions(xmlOut)
  }
}