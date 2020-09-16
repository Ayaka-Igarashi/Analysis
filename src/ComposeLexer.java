import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

// 解析結果をもとに、字句解析器を作る
public class ComposeLexer {

	public static void main(String[] args) {
		System.out.println("> main");
		ComposeLexer.extract();
	}

	// xmlファイルから情報を抜き出す
	public static void extract() {
		try ( InputStream is = Files.newInputStream(Paths.get( "src/output.xml" ))) {
			System.out.println("> import output.xml");
            DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Node root = builder.parse( is );
            readXml( root );
        } catch ( ParserConfigurationException | IOException | SAXException e ) {
            System.out.println( e.getMessage() );
        }
	}

	// 情報をもとに解析器を作る
	public static void compose() {

	}

	// DOMを利用して情報を取得
	private static void readXml( Node node ) {
		String nodename = node.getNodeName();
		if (nodename.equals("coreference")) {
        	return;
        } else if (nodename.equals("parse")) {
        	return;
        }

        Node child = node.getFirstChild();
        while( child != null ) {
        	String childNodeName = child.getNodeName();
        	if (nodename.equals("token")) {
        		if (!childNodeName.equals("word") && !childNodeName.equals("POS")) {
        			child = child.getNextSibling();
        			continue;
                } else {
                	System.out.println("");
            		System.out.print( child.getNodeName() + " =");
                }
        	}

        	if (nodename.equals("word") || nodename.equals("POS")) {
        		if (childNodeName.equals("#text")) {
            		System.out.print(" " + child.getNodeValue() );
            	}
        	}

            readXml( child );
            child = child.getNextSibling();
        }


    }
}
