package carisma.xutils.regulatory.importer.bsi;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStreamWriter;
import java.util.List;

import org.apache.commons.lang3.StringEscapeUtils;
import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class TestPDFParser {
		
		/** represents the content of an article */
	private String currentContent = "";
		/** to store the current header */
	private String currentHeader = "";
		/** to store the sub header */
	private String currentSubHeader = "";
		
	

	public void test(){
		try {
//			System.out.println("testing PDFBox ...");
//				// extract the text from the given pdf file
//			ExtractText.main(new String[]{"-encoding", "UTF-8", "-html", "-sort", "-startPage", "6", "-endPage", "89", 
//										"resources/standard_1002_pdf.pdf", "resources/test.html"});
//			
//			System.out.println("Done ...");
			System.out.println("Refactor the test-file ...");
			
			BufferedReader fileReader = new BufferedReader(new FileReader("resources/test.html"));
			String file_text = "";
			FileOutputStream fos = new FileOutputStream("resources/test2.html");
			OutputStreamWriter streamWriter = new OutputStreamWriter(fos, "UTF-8");
			while(fileReader.ready()){
				file_text = fileReader.readLine();

				// refactoring ... to form a valid html-structure
				file_text = StringEscapeUtils.unescapeHtml4(file_text);
				file_text = file_text.replace("HTML 4.01 Transitional//EN\"", 
											  "HTML 4.01 Transitional//EN\" ");
				file_text = file_text.replace("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">", 
											  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>");
//				System.out.println(file_text);
				streamWriter.write(file_text);
			}
				// close the streams
			fileReader.close();
			streamWriter.flush();
			streamWriter.close();
			System.out.println("Done ...");
			System.out.println("Start parsing the test file ...");
				// the reader to parse the file
			SAXReader reader = new SAXReader(false);
			reader.setEncoding("UTF-8");
			// not validate against a DTD
			try {
				reader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd",false);
//				reader.setFeature("http://xml.org/sax/features/validation",false);
//				reader.setFeature("http://xml.org/sax/features/external-general-entities",false);
//				reader.setFeature("http://apache.org/xml/features/validation/schema",false);
//				reader.setFeature("http://xml.org/sax/features/external-parameter-entities",false);
//				reader.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar",false);
			} catch (SAXException e) {
				System.err.println("Failed to set the Feature!");
				e.printStackTrace();
			}
			Document document = reader.read(new InputSource("resources/test2.html"));
			
			// catch all nodes in the document
			@SuppressWarnings("unchecked")
			List<Node> list = document.selectNodes("//*");
			String nodeText = ""; String value = "";
			for(Node node : list){
				value = "";
				nodeText = node.getText();
//				System.out.println(node.getName());
					// the text of the node must be within a 'p'-object and must not match a pagenumber
				if(node.getName().equals("p") && !(nodeText.startsWith("Seite") || nodeText.startsWith("BSI-Standard"))){
					try{
						value = String.valueOf(Integer.parseInt(nodeText.substring(0, 1)));
					} catch (NumberFormatException n){ /* do nothing, no number in the text */ }
					if(!value.equals("")){
						if(currentHeader.equals("")){
							if(nodeText.matches("[1-9]{1,3}\\.[0-9]{1,3}.*")){
								currentSubHeader = nodeText;
								System.out.println("Sub Header: " + currentSubHeader);
							} else{
								currentHeader = nodeText;
								System.out.println("Header: " + currentHeader);
							}
						} else if(!nodeText.startsWith(currentHeader.substring(0, currentHeader.length()/2))){
							if(nodeText.matches("[1-9]{1,3}\\.[0-9]{1,3}.*")){
								currentSubHeader = nodeText;
								System.out.println("Sub Header: " + currentSubHeader);
							} else{
								currentHeader = nodeText;
								System.out.println("Header: " + currentHeader);
							}
						} else if(nodeText.length() > 100){
							if(nodeText.startsWith(currentHeader.substring(0, currentHeader.length()/2))){
								nodeText = nodeText.substring(currentHeader.length()-1, nodeText.length());
								currentContent = nodeText;
								System.out.println("Zusammengeschusterter Text: " + currentContent);
							}
						}
					} else{
						if(!nodeText.matches("([A-Za-z]|[0-1]).*")){
							if(nodeText.startsWith("Ü") || nodeText.startsWith("Ä") || nodeText.startsWith("Ö")){
								currentContent = nodeText;
								System.out.println("Content: " + currentContent);
							} else{
								currentContent = nodeText;
								System.out.println("Other than content: " + currentContent);
							}
						} else{
							currentContent = nodeText;
							System.out.println("Content: " + currentContent);
						}
						
					}
				}
			}
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (InterruptedException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (InstantiationException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (IllegalAccessException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public static void main(String[] args){
		TestPDFParser testPDFParser = new TestPDFParser();
		testPDFParser.test();
	}

}
