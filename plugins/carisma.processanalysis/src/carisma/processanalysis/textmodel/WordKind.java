package carisma.processanalysis.textmodel;

public enum WordKind {
	/** Originaltext aus Prozessbeschreibung **/
	SOURCE,
	/** The extension is a cooccurrence of the annotated word. */
	COOCCURRENCE,
	/** The extension is a synonyme. */
	SYNONYME,
	/** ein *uebergeordnetes* Keyword aus der eigenen Taxonomie **/
	TAXONOMYPREDECESSOR,
	PATTERNNAME, 
	PATTERNTITLE, 
	PATTERNTEXT, 
	PATTERNKEYWORD,
	/** The type of the extension is unknown. */
	UNKNOWN
}
