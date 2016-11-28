package carisma.ui.vision.questions;


import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_QUESTION_COLLECTION;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_QUESTION_DOCUMENT;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_QUESTION_FIELD;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_SECRET;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_URL;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.KEY_USER;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.preference.IPreferenceStore;
import org.w3c.dom.Document;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.io.content.XML_DOM;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;

public class QuestionGenerationAction extends Action {
	
	private AnalysisResult analyisResult;
	
	public QuestionGenerationAction(AnalysisResult analysisResult){
		this.analyisResult = analysisResult;
	}
	
	
	
	@Override
	public void run() {
		super.run();
		
		//A Question list for all Questions from all checks
		List<Question> questionsList = new ArrayList<Question>();
		//A builder list for all Builder for all checks 
		List<Builder> b = new ArrayList<Builder>();
		//A BuilderFactory for getting all the builders
		b = BuilderFactory.getBuilder(this.analyisResult);
		//tests if there is no builder and throws an exception
		if (b.size() < 1){
			throw new IllegalArgumentException("No Builder for this checks!");
		}
		//Instantiating the list of Question
		for(int i = 0; i < b.size(); i++){
			List<Question> questions = b.get(i).generateQuestion();
			for(int j = 0; j<questions.size(); j++){
				questionsList.add(questions.get(j));
			}
		}
		//Instantiating an Questions Element with the list of question
		Questions questionsAll = new Questions();
		questionsAll.setQuestions(questionsList);
		
		try {
			//function for generating the XML_DOM document with a Questions Element
			XML_DOM questionsXmlDom = buildQuestionsXml(questionsAll);
			
			//function for saving the XML document in the database
			writeDB(questionsXmlDom);
		}
		catch (JAXBException e){
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}
	}

	//function for generating the XML_DOM document with a Questions Element
	public static XML_DOM buildQuestionsXml(Questions questionsAll) throws ParserConfigurationException, JAXBException {
		
		//building the document
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
	    dbf.setNamespaceAware(true);
	    DocumentBuilder db =  dbf.newDocumentBuilder();
	  
	    //document for the XML_DOM questions
		Document questionsXml = db.newDocument();
		//instantiating the document with the questions
		JAXBContext jc = JAXBContext.newInstance(Questions.class);
		Marshaller m = jc.createMarshaller();
		m.marshal( questionsAll, questionsXml );
		
		XML_DOM xml = new XML_DOM(questionsXml);
		return xml;
	}
	
	private static void writeDB(XML_DOM questionsXmlDom){
		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String user = preferencesStore.getString(KEY_USER);
		String secret = preferencesStore.getString(KEY_SECRET);
		String url = preferencesStore.getString(KEY_URL);

		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);

		String questionCollection = preferencesStore.getString(KEY_QUESTION_COLLECTION);
		String questionDocument = preferencesStore.getString(KEY_QUESTION_DOCUMENT);
		String questionField = preferencesStore.getString(KEY_QUESTION_FIELD);
		MongoDBDestination carismaConfiguration = new MongoDBDestination(questionCollection, questionDocument, questionField);
		boolean success = db.write(carismaConfiguration, questionsXmlDom);
		StringBuilder errorMessageBuilder = new StringBuilder();
		if (!success) {
			String response = db.getResponseMessage().toString();

			errorMessageBuilder.append("Export of the questions failed for the following reason:");
			errorMessageBuilder.append(response);
			errorMessageBuilder.append("\n");
		}
	}
	
	
}


