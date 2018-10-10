package carisma.ui.vision.test.questiongeneration;



import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.CheckResult;
import carisma.core.analysis.result.StatusType;
import carisma.core.io.content.XML_DOM;


public class CheckResultJAXBTest {

	@SuppressWarnings("static-method")
	@Test
	public void test() {
		//initialize a checkResult
		CheckResult checkResult = new CheckResult();
		checkResult.setName("UMLsec secure links Check");
		checkResult.setSuccessful(false);
		
		StatusType status = null;
		String text = "default attacker can read data";
		AnalysisResultMessage aRM = new AnalysisResultMessage(status, text);
		
		checkResult.addResult(aRM);
		
		//build a document with this checkResult
		Document checkResultDocument = null;
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
	    dbf.setNamespaceAware(true);
	    DocumentBuilder db = null;
		try {
			db = dbf.newDocumentBuilder();
			checkResultDocument = db.newDocument();
		} catch (ParserConfigurationException e1) {
			e1.printStackTrace();
		}
		try {
			JAXBContext jc = JAXBContext.newInstance(CheckResult.class);
			Marshaller m = jc.createMarshaller();
			m.marshal( checkResult, checkResultDocument );
		} catch (JAXBException e) {
			e.printStackTrace();
		}
		
		//build a XML_DOM with this document
		XML_DOM xml1 = new XML_DOM(checkResultDocument);
		System.out.println(xml1.asString());
		
		//unmarshall a checkResult from the xml-document
		CheckResult jaxbCheckResult1 = null;
		JAXBContext jaxbContext;
		try {
			jaxbContext = JAXBContext.newInstance(CheckResult.class);
			Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			jaxbCheckResult1 = (CheckResult) jaxbUnmarshaller.unmarshal(checkResultDocument); 
		} catch (JAXBException e) {
			e.printStackTrace();
			throw new AssertionError();
		}
		

		//test if the unmarshalled checkResult is the same as the actual
		Assert.assertTrue(checkResult.getName().equals(jaxbCheckResult1.getName()));
		Assert.assertTrue(checkResult.isSuccessful() == jaxbCheckResult1.isSuccessful());
		for(int i = 0; i<checkResult.getResults().size(); i++){
			Assert.assertTrue(checkResult.getResults().get(i).getText().equals(jaxbCheckResult1.getResults().get(i).getText()));
			Assert.assertTrue(checkResult.getResults().get(i).getStatus() == jaxbCheckResult1.getResults().get(i).getStatus());
		}
		
	}

}
