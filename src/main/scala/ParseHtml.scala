import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter, PrintWriter}

import pine.HtmlParser

// HTML文章の解析をする
object ParseHtml {
  def parseHtml() = {

    System.out.println("> html_parse_start")

    val file: BufferedReader = new BufferedReader(new FileReader(new File("src/inputHTML.txt")))
    //val xmlOut = new PrintWriter(new BufferedWriter(new FileWriter(new File("src/outputHTML.xml"))))
    val htmlOut = new PrintWriter(new BufferedWriter(new FileWriter(new File("src/outputHTML.txt"))))

    val str: String = file.readLine()

    val html = str
    val node = HtmlParser.fromString(html)

    htmlOut.println(node)

    htmlOut.close()

    System.out.println("> html_parse_fin")
  }
}
