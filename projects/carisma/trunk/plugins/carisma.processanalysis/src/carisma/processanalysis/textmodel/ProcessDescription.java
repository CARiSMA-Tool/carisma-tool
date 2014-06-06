package carisma.processanalysis.textmodel;

import java.util.ArrayList;

/**
 * This class is the central data structure the RiskFinder deals with.
 * 
 * @author Christian Wessel
 * 
 */
public class ProcessDescription {
	
	public static final String CARISMA_REGISTRY_KEY = "carisma.data.processanalysis.process";
	/**
	 * Contains all words that could be extracted from a model.
	 */
	private ArrayList<ProcessEntity> entities = new ArrayList<ProcessEntity>();
	
	private String origin = null;

	/**
	 * The Default constructor.
	 */
	public ProcessDescription() {
		
	}

	/**
	 * The Default constructor.
	 */
	public ProcessDescription(final String origin) {
		this.origin = origin;
	}
	
	/**
	 * Adds one model entity (activity) to the model.
	 * @param entity the entity to be added.
	 */
	public final void addEntity(final ProcessEntity entity) {
		entities.add(entity);
	}

	/**
	 * Retrieves all stored entities.
	 * @return A list of model entities.
	 */
	public final ArrayList<ProcessEntity> getEntities() {
		return entities;
	}

	/**
	 * 
	 * Replaces all stored entities.
	 * @param entities A list of entities to be stored in the model.
	 */
	public final void setEntities(final ArrayList<ProcessEntity> entities) {
		this.entities = entities;
	}

	public String getOrigin() {
		return origin;
	}

	public void setOrigin(String origin) {
		this.origin = origin;
	}
	
	/**
	 * Nur ein Workaround, solange wir keine Umlaute aus BPMN-Modellen lesen können
	 */
	public void addUmlaute(){
		for(ProcessEntity curEntity : this.entities){
			for(Text curText : curEntity.getTexts()){
//				ArrayList<Word> newWords = new ArrayList<Word>();
//				String newText = "";
//				for(Word curWord : curText.getWordList()){
//					String value = curWord.getContent();
//					String modValue = value.replace("ae", "ä").replace("oe", "ö").replace("ue", "ü");
//					if(!value.equalsIgnoreCase(modValue)){ //TODO: nicht ideal, Worte können trotzdem doppelt vorkommen
//						newText += " " + modValue;
////						newWords.add(new Word(modValue));
//					}
//				}
////				curText.getWordList().addAll(newWords);
//				curText.setEntityText(curText.getEntityText() + newText);
//				curText.tokenize();
				
				String oldText = curText.getEntityText();
				String newText = oldText.replace("ae", "ä").replace("oe", "ö").replace("ue", "ü").replace("Ae", "Ä").replace("Oe", "Ö").replace("Ue", "Ü");
				if(!oldText.equals(newText)){
					curText.setEntityText(newText);
					curText.tokenize();
				}					
			}
		}
	}
}
