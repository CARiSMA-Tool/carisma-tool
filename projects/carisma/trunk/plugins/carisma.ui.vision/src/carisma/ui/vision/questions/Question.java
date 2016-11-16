package carisma.ui.vision.questions;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "question")
public class Question {


  private String attacker;
  private String ability;
  private String commPath;
  private String text;
  
 
  @XmlElement(name="text")
  public String getText() {
	return text;
}

  
  public void setText(String text) {
	this.text = text;
  } 

  @XmlElement(name="commPath")
  public String getCommPath() {
	return commPath;
  }

  public void setCommPath(String commPath) {
	this.commPath = commPath;
  }

  @XmlElement(name="attacker")
  public String getAttacker() {
	return attacker;
  }
  
  public void setAttacker(String outputAttacker) {
	this.attacker = outputAttacker;
  }
  
  @XmlElement(name="ability")
  public String getAbility() {
	return ability;
  }
  
  public void setAbility(String ability) {
	this.ability = ability;
  }

 
}
