package carisma.processanalysis.textmodel;

// PROCESS... sind alle Texte, die aus einer Prozessbeschreibung stammen (s. Textmodel etc.)
// PATTER... sind entspr. Texte aus verschiedenen Arten von Pattern

public enum TextKind {
	PROCESSNAME,
	PROCESSDESCRIPTION,
	PROCESSCOMMENT,
	PATTERNNAME,
	PATTERNTITLE,
	PATTERNTEXT,
	OTHER
}
