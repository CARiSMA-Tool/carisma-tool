package carisma.xutils.regulatory.importer.juris;
/**
 * 
 * @author dbuerger
 * this class is a representation of the cross connection of an article to another (probably in a different code of law)
 *
 */
public class LawCrossConnection {
		/** the number of the paragraph */
	private String paragraph = "";
		/** the article number */
	private String article;
		/** the code of law which the article is related to */
	private String codeOfLaw;
		/** the section with its article */
	private String section;
	
	/**
	 * Constructor of the LawCrossConnection
	 * 
	 * @param paragraph the number of the paragraph
	 * @param article the number of the article
	 * @param codeOfLaw the code of law where the article is related to
	 * @param section the section to which the article belongs
	 */
	public LawCrossConnection(String paragraph,String article, String codeOfLaw ,String section){
		this.paragraph = paragraph;
		this.article = article;
		this.codeOfLaw = codeOfLaw;
		this.section = section;
	}
	/**
	 * Default Constructor. All parameters have to be set afterwards 
	 */
	public LawCrossConnection(){ }
		/**
		 * @return the paragraph
		 */
		public String getParagraph() {
			return paragraph;
		}
		/**
		 * @param paragraph the paragraph to set
		 */
		public void setParagraph(String paragraph) {
			this.paragraph = paragraph;
		}
		/**
		 * @return the article
		 */
		public String getArticle() {
			return article;
		}
		/**
		 * @param article the article to set
		 */
		public void setArticle(String article) {
			this.article = article;
		}
		/**
		 * @return the codeOfLaw
		 */
		public String getCodeOfLaw() {
			return codeOfLaw;
		}
		/**
		 * @param codeOfLaw the codeOfLaw to set
		 */
		public void setCodeOfLaw(String codeOfLaw) {
			this.codeOfLaw = codeOfLaw;
		}
		/**
		 * @return the section
		 */
		public String getSection() {
			return section;
		}
		/**
		 * @param section the section to set
		 */
		public void setSection(String section) {
			this.section = section;
		}
		/**
		 * prints the law object
		 */
		public void printLawCrossConnection(){
			System.out.println("Der Paragraph enthaelt die Verbindung zu dem Artikel "+getSection()+" " +
					"mit dem Paragraphen "+getArticle()+" Absatz "+getParagraph()+" aus dem Gesetz "+codeOfLaw);
		}
		
}	