package carisma.ui.vision.test.questiongeneration;



import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.w3c.dom.Document;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.io.content.XML_DOM;
import carisma.core.io.implementations.FileIO;
import carisma.ui.vision.questions.Question;
import carisma.ui.vision.questions.QuestionGenerationAction;
import carisma.ui.vision.questions.Questions;
import carisma.ui.vision.questions.SecureLinksBuilder;

@RunWith(Parameterized.class)
public class QuestionsGenerationTestFail {
	
	 private AnalysisResult analysisResult;
	 private XML_DOM qTest;
	 private SecureLinksBuilder secureLinksBuilder;
	 
	 
	 public QuestionsGenerationTestFail(AnalysisResult analysisResult, XML_DOM qTest) {
		super();
		this.analysisResult = analysisResult;
		this.qTest = qTest;
	}
	 

	 @Before
	   public void initialize() {
		 this.secureLinksBuilder = new SecureLinksBuilder(this.analysisResult);		 
	   }

	   @Parameterized.Parameters
	   public static Collection<Object[]> documents() {
		  File file1 = new File("data/questionsFail.xml");
		   
		  Document doc1 = FileIO.read(file1);
		  XML_DOM xml1 = new XML_DOM(doc1); 
				   
		  AnalysisResult jaxbAnalysisResult1 = null; 
		  File file2 = new File("data/SecureLinksFail1.xml");
		  Document doc2 = FileIO.read(file2);
		  
		  JAXBContext jaxbContext;
		  try {
			  jaxbContext = JAXBContext.newInstance(AnalysisResult.class);
			  Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			  jaxbAnalysisResult1 = (AnalysisResult) jaxbUnmarshaller.unmarshal(doc2); 
		  } catch (JAXBException e) {
			  e.printStackTrace();
		  }
		  
				   
		  AnalysisResult jaxbAnalysisResult2 = null; 
		  File file4 = new File("data/SecureLinksFail2.xml");
		  Document doc4 = FileIO.read(file4);
		   

		  try {
			  jaxbContext = JAXBContext.newInstance(AnalysisResult.class);
			  Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			  jaxbAnalysisResult2 = (AnalysisResult) jaxbUnmarshaller.unmarshal(doc4); 
		  } catch (JAXBException e) {
			  e.printStackTrace();
		  }
		  
		  AnalysisResult jaxbAnalysisResult3 = null; 
		  File file5 = new File("data/SecureLinksFail3.xml");
		  Document doc5 = FileIO.read(file5);
		   

		  try {
			  jaxbContext = JAXBContext.newInstance(AnalysisResult.class);
			  Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			  jaxbAnalysisResult3 = (AnalysisResult) jaxbUnmarshaller.unmarshal(doc5); 
		  } catch (JAXBException e) {
			  e.printStackTrace();
		  }
		   
		  return Arrays.asList(new Object[][] {
			  { jaxbAnalysisResult1, xml1 },
			  { jaxbAnalysisResult2, xml1 },
			  { jaxbAnalysisResult3, xml1 },
	      });
	   }


	   @Test(expected=IllegalArgumentException.class) 
	   public void test() throws ParserConfigurationException, JAXBException {
	      System.out.println("AnalysisResult is : " + this.analysisResult);
	      List<Question> questionList = this.secureLinksBuilder.generateQuestion();
	      Questions questions = new Questions();
	      questions.setQuestions(questionList);
	      XML_DOM questionsXmlDom = QuestionGenerationAction.buildQuestionsXml(questions);
	      System.out.println(this.qTest.asString());
	      System.out.println(questionsXmlDom.asString());
	      Document doc1 = this.qTest.getDocument();
	      Document doc2 = questionsXmlDom.getDocument();
	      doc1.normalize();
	      doc2.normalize();
	      Assert.assertTrue(doc1.isEqualNode(doc2));
	   }
	


	
		
}
