package carisma.ui.vision.questions;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "questions")
public class Questions {

	private List<Question> question = new ArrayList<>();

	@XmlElement(name = "question")
	public List<Question> getQuestions() {
		return this.question;
	}

	public void setQuestions(List<Question> question) {
		this.question = question;
	}
	
	public void addQuestion(Question question){
		this.question.add(question);
	}
}
