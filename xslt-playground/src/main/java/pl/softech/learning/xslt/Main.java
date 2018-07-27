package pl.softech.learning.xslt;

import org.w3c.dom.Document;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.File;
import java.net.URL;

/**
 * @author sledzs
 * @since 27.07.18
 */
public class Main {

    public static void main(String[] args) throws Exception {

        URL request = Main.class.getClassLoader().getResource("request.xml");
        URL transformation = Main.class.getClassLoader().getResource("notifications.xslt");

//        System.out.println(request);
//        System.out.println(transformation);

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        File stylesheet = new File(Main.class.getClassLoader().getResource("notifications.xslt").getFile());
        File datafile = new File(Main.class.getClassLoader().getResource("request.xml").getFile());

        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(datafile);

        // Use a Transformer for output
        TransformerFactory tFactory = TransformerFactory.newInstance();
        StreamSource stylesource = new StreamSource(stylesheet);
        Transformer transformer = tFactory.newTransformer(stylesource);

        DOMSource source = new DOMSource(document);
        StreamResult result = new StreamResult(System.out);
        transformer.transform(source, result);
    }

}
