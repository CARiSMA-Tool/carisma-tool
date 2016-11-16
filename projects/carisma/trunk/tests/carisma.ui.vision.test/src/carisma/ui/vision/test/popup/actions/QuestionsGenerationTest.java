package carisma.ui.vision.test.popup.actions;


import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.w3c.dom.Document;

import carisma.core.io.content.XML_DOM;
import carisma.core.io.implementations.FileIO;
import carisma.ui.vision.questions.Question;
import carisma.ui.vision.questions.QuestionGenerationAction;
import carisma.ui.vision.questions.Questions;
import carisma.ui.vision.questions.SecureLinksBuilder;
import carisma.core.analysis.result.AnalysisResult;



@RunWith(Parameterized.class)
public class QuestionsGenerationTest{
	 private AnalysisResult analysisResult;
	 private XML_DOM qTest;
	 private SecureLinksBuilder secureLinksBuilder;
	 private QuestionGenerationAction qO;
	 
	 
	 public QuestionsGenerationTest(AnalysisResult analysisResult, XML_DOM qTest) {
		super();
		this.analysisResult = analysisResult;
		this.qTest = qTest;
	}
	 

	 @Before
	   public void initialize() {
		 secureLinksBuilder = new SecureLinksBuilder(analysisResult);
		 qO = new QuestionGenerationAction(analysisResult);
		 
	   }

	   @Parameterized.Parameters
	   public static Collection<Object[]> documents() {
		  File file1 = new File("data/questionsFail.xml");
		   
		  Document doc1 = FileIO.read(file1);
		  XML_DOM xml1 = new XML_DOM(doc1); 
				   
		  AnalysisResult jaxbAnalysisResult1 = null; 
		  File file2 = new File("data/SecureLinksFail.xml");
		  Document doc2 = FileIO.read(file2);
		  
		  JAXBContext jaxbContext;
		  try {
			  jaxbContext = JAXBContext.newInstance(AnalysisResult.class);
			  Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			  jaxbAnalysisResult1 = (AnalysisResult) jaxbUnmarshaller.unmarshal(doc2); 
		  } catch (JAXBException e) {
			  e.printStackTrace();
		  }
		  
		  File file3 = new File("data/questionsSuccess.xml");
		   
		  Document doc3 = FileIO.read(file3);
		  XML_DOM xml2 = new XML_DOM(doc3); 
				   
		  AnalysisResult jaxbAnalysisResult2 = null; 
		  File file4 = new File("data/SecureLinksSuccess.xml");
		  Document doc4 = FileIO.read(file4);
		   

		  try {
			  jaxbContext = JAXBContext.newInstance(AnalysisResult.class);
			  Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
			  jaxbAnalysisResult2 = (AnalysisResult) jaxbUnmarshaller.unmarshal(doc4); 
		  } catch (JAXBException e) {
			  e.printStackTrace();
		  }
		   
		  return Arrays.asList(new Object[][] {
			  { jaxbAnalysisResult1, xml1 },
			  { jaxbAnalysisResult2, xml2 },
	      });
	   }


	   @Test
	   public void test() {
	      System.out.println("AnalysisResult is : " + analysisResult);
	      List<Question> questionList = secureLinksBuilder.generateQuestion();
	      Questions questions = new Questions();
	      questions.setQuestions(questionList);
	      XML_DOM questionsXmlDom = qO.buildQuestionsXml(questions);
	      System.out.println(qTest.asString());
	      System.out.println(questionsXmlDom.asString());
	      String s1 = qTest.asString().trim();
	      String s2 = questionsXmlDom.asString().trim();
	      Assert.assertTrue(s1.compareTo(s2) == 0);
	   }
	

}
