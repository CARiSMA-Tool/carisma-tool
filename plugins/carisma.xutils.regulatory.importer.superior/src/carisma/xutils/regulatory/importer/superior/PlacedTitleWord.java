package carisma.xutils.regulatory.importer.superior;

// wird im Rulelemenetscreator benutzt, um die Positionen von Worten auch über die Baseform-Bildung hinaus zu erhalten

public class PlacedTitleWord {
	private String originalWord;
	private String baseformedWord;
	private int startIndex;
	private int endIndex;

	public PlacedTitleWord(String originalWord, String baseformedWord,
			int startIndex, int endIndex) {
		super();
		this.originalWord = originalWord;
		this.baseformedWord = baseformedWord;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
	}

	public String getOriginalWord() {
		return originalWord;
	}
	public void setOriginalWord(String originalWord) {
		this.originalWord = originalWord;
	}
	public String getBaseformedWord() {
		return baseformedWord;
	}
	public void setBaseformedWord(String baseformedWord) {
		this.baseformedWord = baseformedWord;
	}
	public int getStartIndex() {
		return startIndex;
	}
	public void setStartIndex(int startIndex) {
		this.startIndex = startIndex;
	}
	public int getEndIndex() {
		return endIndex;
	}
	public void setEndIndex(int endIndex) {
		this.endIndex = endIndex;
	}
	

}
