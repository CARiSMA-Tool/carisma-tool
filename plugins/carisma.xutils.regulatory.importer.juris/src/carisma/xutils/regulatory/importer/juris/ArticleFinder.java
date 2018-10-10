package carisma.xutils.regulatory.importer.juris;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * @author dbuerger
 * 
 * this class is used to find connections between articles in a given string
 * actually all mentioned articles are stored
 * 
 */
public class ArticleFinder {
		/** wildcard for paragraphs */
	public static final String PARAGRAHP = "(((Absatz) \\d{1,4})|((Abs.?) \\d{1,4}))?";
		/** wildcard for sentence numbers */
	public static final String SENTENCENUMBER = "(((Nr.?)|(Nummer)) \\d{1,4})?"; /* ((Nr.?)|(Nummer) \\d{1,4})? */
		/** wildcard for article numbers */
	public static final String ARTICLE = "\\d{1,4}[a-z]?";
		/** wildcard for different code of law */
	public static final String LAW = "([^\\.]*(.*?!Artikel.*)?(([gG]esetz\\p{Lower}{2,5})|([oO]rdnung )))?";
		/** wildcard for the suffix of the concerned passage */	
	public static final String SUFFIX = LAW + "([^§\\.]*[rR]ichtlinie \\d{1,3}/\\d{1,2}/[A-Z]{1,3}( )?)?([^§\\.]*[oO]rdnungs[a-z]{2,13})?" +
								  ")";
//								  "([^§\\.]*(?!und nicht)[nN]icht)?)"; 
		/** regular expression to search for mentioned articles in the given law entry	 */
	private static String regex = "";
	
	
	/**
	 * Constructor of the ArticleFinder
	 */
	public ArticleFinder(){
			/*
			 * part considers that any § is followed by an article number
			 */
		regex = "((entgegen )?((§§)|(§)) "+ARTICLE+"( )?"+PARAGRAHP+"( )?"+SENTENCENUMBER+"" +		
			/* 
			 * part to control if specific words are contained
			 */
			 	"(((§)|(, )|(und)|(bis)|(oder)|(!Satz)|(in Verbindung mit)|(sowie))?( )?("+ARTICLE+")?( )?"+PARAGRAHP+"( )?"+SENTENCENUMBER+"( )?)*" + 	
			 /*
			  * part to control if the article points on a different code of law 
			  */
			  	""+SUFFIX+"|" +
			 /*
			  * part to find sections with their considering articles. Structure is similar to the first part above
			  */
			  "(((Art.?)|(Artikel)) "+ARTICLE+" " +
				"(((§)|(, )|(und)|(bis)|(oder))?(!Satz)?( )?("+ARTICLE+")?( )?"+PARAGRAHP+"( )?"+SENTENCENUMBER+"( )?)*"+SUFFIX;
	}
	
	/**
	 * searches in a string for matches with a given regular expression
	 * the resulting strings were stored in a list  
	 * 
	 * @param regex regular expression
	 * @param string String
	 * @return list of the resulting strings
	 */
	private static ArrayList<String> checkRegex(String regex, String string){
		ArrayList<String> list = new ArrayList<String>();
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(string);
		while(m.find()) {
			list.add(m.group());
//			divideArticle(list.get(list.size()-1));
		}
		return list;
	}
	
	/**
	 * checks if a string matches a regular expression
	 * 
	 * @param regex the regular expression
	 * @param string the string to be test
	 * @return true if string matches the expression
	 */
	private static boolean containsRegex(String regex, String string){
		boolean matches = false;
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(string);
		if(m.matches()){ matches =  true; }
		return matches;
	}
	/**
	 * parses the string and searches for valid connections to other articles.
	 * even the relations to other codes of law were stored and all results
	 * were given back in a list of connection-objects
	 * 
	 * @param string the string to be parsed
	 * @return the list
	 */
	public ArrayList<LawCrossConnection> getLawCrossConnections(String string){
		String section = ""; String codeOfLaw = "";
		boolean contains = false;
		ArrayList<String> articles = null; String[] articleNumber = null;
		ArrayList<LawCrossConnection> lawConnections = new ArrayList<carisma.xutils.regulatory.importer.juris.LawCrossConnection>();
		ArrayList<LawCrossConnection> lawCrossConnections = new ArrayList<carisma.xutils.regulatory.importer.juris.LawCrossConnection>();
		String notContained = ".*([nN]icht|keine).*";		// regex to find valid articles
		LawCrossConnection crossConnection = null;
		ArrayList<String> lawList = checkRegex(regex, string);
//		for(String str : lawList){ 
//			System.out.println("getLawCrossConnection "+str);
//		}
		if(!lawList.isEmpty()){
			for(String law : lawList){
					/* check if there are two sentences in the result from the regular expression */
				if(containsRegex(".*\\. [A-Z].*", law)){ law = law.split("\\. [A-Z]")[0]; }
					/*check if there are different paragraphs */
				if(containsRegex(".*\\([0-9]{1,2}\\).*", law)){ law = law.split("\\([0-9]{1,2}\\)")[0]; }
//				System.out.println("String "+law);
				if(!containsRegex(notContained, law) | law.contains("entgegen") | containsRegex(".*(und nicht).*", law)){
						/* if there mentioned law is valid and no offence is present */
//					System.out.println("VALID "+law);
					section = getSection(law);
					codeOfLaw = getCodeOfLaw(law);
					if(!section.equals("")){
							/* Split the string at the end of the mentioned section */
						law = law.substring(law.indexOf("Artikel "+section)+8+section.length()+1, law.length());
//						System.out.println(law);
					} else { /* do nothing */ }
					articles = divideArticle(law);
					if(!articles.isEmpty()){
						for(String str : articles){
							articleNumber = getArticleNumber(str);
							crossConnection = new LawCrossConnection(articleNumber[1], articleNumber[0], codeOfLaw, section);
							lawCrossConnections.add(crossConnection);
							crossConnection = null;
						}
					} else{
						crossConnection = new LawCrossConnection("", "", codeOfLaw, section);
						lawCrossConnections.add(crossConnection);
						crossConnection = null;
					}
				} else{
						/* if the mentioned law is not valid */
				}
			}
		}
			/* check if the article is mentioned twice */ 
		for(LawCrossConnection law : lawCrossConnections){
			contains = false;
			if(lawConnections.isEmpty()){ lawConnections.add(law); contains = true; }
			else {
				for(LawCrossConnection lawTest : lawConnections){
						/* the article is only mentioned twice if all parameters are equal */ 
					if(law.getArticle().equals(lawTest.getArticle()) &&
							law.getCodeOfLaw().equals(lawTest.getCodeOfLaw()) &&
							law.getParagraph().equals(lawTest.getParagraph()) &&
							law.getSection().equals(lawTest.getSection())){
						contains = true;
						/* 
						 * if there is another connection to an article which is only related
						 * to another paragraph, then the connection will be ignored
						 */
					} else if(law.getArticle().equals(lawTest.getArticle()) &&
							law.getCodeOfLaw().equals(lawTest.getCodeOfLaw()) &&
							law.getSection().equals(lawTest.getSection()) &&
							(law.getParagraph().equals("") | lawTest.getParagraph().equals(""))){
						contains = true;						
					}
				}
			}
			if(!contains){ lawConnections.add(law); }
		}
//		for(LawCrossConnection law : lawConnections){
//			law.printLawCrossConnection();
//		}
		return lawConnections;
	}
	
		
	/**
	 * Retrieves a number out of a char array.
	 * It only finds the first number which is present in the string.
	 * The suffix specifies if a suffix can be found (e.g. 24a) 
	 * 
	 * 
	 * @param array the char array
	 * @param suffix specifies if a suffix should be found
	 * @return the first found integer as string
	 */
	private static String getInteger(char[] array, boolean suffix){
		String integer = "";
		for(int i=0; i<array.length; i++){
			try{	// parse the given char array to find integers
				integer += String.valueOf(Integer.parseInt(String.valueOf(array[i])));
			} catch (NumberFormatException n){ 
					// if a suffix should be found
				if(integer.length() > 0 && suffix == true){
					if(String.valueOf(array[i]).equals(" ")){ /* do nothing */ }
					else { integer += String.valueOf(array[i]); }
				}
				if(integer.length() > 0){ 
					return integer;
				}
			}	
		} return integer;
	}
	
	/**
	 * divides the given string in parts which belong to different articles.
	 * when a range of articles is mentioned, all the related articles are stored.
	 * 
	 * @param string the string to parse
	 * @return a list with different articles
	 */
	private ArrayList<String> divideArticle(String string){
//		System.out.println("divide Article "+string);
		ArrayList<String> paraList = new ArrayList<String>();
		String[] list = null;
			/* divides the string in parts, which are seperated by '§' */
		if(string.contains(" §")){
			list = string.split(" §");
			for(int i=0; i<list.length; i++){
				paraList.addAll(divide(list[i]));				
			}
		} else{ 
			paraList.addAll(divide(string));
		}
//		for(String str : paraList){ System.out.println(str); }
		return paraList;
		
	}
	
	/**
	 * helper method to divide the articles
	 * 
	 * @param string string to be divided
	 * @return List of articles
	 */
	private ArrayList<String> divide(String string){
		ArrayList<String> paraList = new ArrayList<String>();
		String regex = ".*((Abs)|(Satz)|(Nr)|(Nummer)).*";
		
		if(!containsRegex(regex, string)){
			paraList.addAll(getNumbers(string));
		} else{ 
				/* 
				 * if the part of the string contains a paragraph or sentence
				 */
			boolean isEnd = false;
			while (!isEnd){
				try{
						/* divide the string in two parts. The one with the paragraphs and the other */
					String withOutParagraph = string.substring(0, string.indexOf("Abs")-1);
					withOutParagraph = withOutParagraph.substring(0, withOutParagraph.lastIndexOf(" "));
					string = string.substring(string.indexOf(withOutParagraph)+withOutParagraph.length()+1);
						// if the string ends with ',' we have to delete it
					if(withOutParagraph.endsWith(",")){
						withOutParagraph = withOutParagraph.substring(0, withOutParagraph.lastIndexOf(","));
					}
						/* to filter if the string contains only one article */
					if(withOutParagraph.length() > 2){
						paraList.addAll(getNumbers(withOutParagraph));
					}
						/* to check if there are more paragraphs which relate to one article */
					String substring = string.substring(0, string.indexOf("Abs")+3);
					if(!string.contains(regex)){
							/* get the numbers of the related paragraphs */
						ArrayList<String> paragraphList = getNumbers(string.substring(string.indexOf(substring)+substring.length()));
						for(String str : paragraphList) { 
								/* and add them separated to the list of articles */
							paraList.add(substring+" "+getInteger(str.toCharArray(), false));
						}
						isEnd = true;							
					}
				} catch (IndexOutOfBoundsException e){
					paraList.add(string);
					isEnd = true;
				}
			}
		}
//		for(String str : paraList){ System.out.println("paralist "+str); }
		return paraList;
	}
	
	/**
	 * search for a different code of law
	 * 
	 * @param string string to be searched in
	 * @return the code of law 
	 */
	private String getCodeOfLaw(String string){
//		System.out.println("code of law "+string);
			/* the code of law which will be returned */
		String law = "";
			/* if the string contains a reference to another code of law */
		if(containsRegex(".*esetz.*", string)){
			if(containsRegex(".* [Gg]esetz.*", string)){
				if(containsRegex(".*esetzbuch.*", string)){
					law = string.substring(0, string.indexOf("esetzbuch")+9);
					if(law.toCharArray()[law.lastIndexOf(" ")+1] == "G".toCharArray()[0]){
						String suffix = law.substring(law.lastIndexOf(" ")+1, law.length());
						law = law.substring(0, law.indexOf(" "+suffix));
						String praefix = law.substring(law.lastIndexOf(" ")+1, law.length());
						law = praefix+" "+suffix;
					} else{
						law = law.substring(law.lastIndexOf(" ")+1);
					}
				} else {
					try{
						law = string.substring(string.indexOf("Gesetz"));
					} catch (IndexOutOfBoundsException e){
						// there ist no 'Gesetz' in the text, only like 'gesetzliche'
						// the paragraph is stored
					}
				}
			} else {
				if(containsRegex(".*esetzbuch.*", string)){
					law = string.substring(0, string.indexOf("esetzbuch")+9);
					law = law.substring(law.lastIndexOf(" ")+1);
				} else {
					law = string.substring(0, string.indexOf("esetz")+5);
					law = law.substring(law.lastIndexOf(" ")+1);
				}
			}
			// if the string contains a reference to another infraction
		} else if(containsRegex(".*rdnung.*", string)){
			if(containsRegex(".* Ordnung.*", string)){
				law = string.substring(string.indexOf("rdnung"));
				try{
					law = law.substring(0, law.indexOf(" "));
				} catch (Exception e){ /* do nothing, string found */ }
			} else if(!containsRegex(".* ordnungsgem.*", string)){
				law = string.substring(0, string.indexOf("rdnung")+6);
				law = law.substring(law.lastIndexOf(" ")+1);
			}
		}
		return law;
	}

	/**
	 * splits the articles in article number and paragraph number
	 * 
	 * @param string the article to split
	 * @return array with article and paragraph (or null)
	 */
	private String[] getArticleNumber(String string){
		String[] articleArray = {"", ""};
		articleArray[0] = getInteger(string.toCharArray(), true);
		try{
			String substring = string.substring(string.indexOf("Abs"));
			articleArray[1] = getInteger(substring.toCharArray(), false);
		} catch (Exception e){ /* do nothing, no paragraph found */ }
		return articleArray;
	}
	
	/**
	 * searchs if the string starts with a section.
	 * if it does not, then an empty string is responsed
	 * 
	 * @param string the string to be searched in
	 * @return the result
	 */
	private String getSection(String string){
		String section = "";
		if(string.startsWith("Artikel")){
			section = getInteger(string.toCharArray(), true);
		} else { /* do nothing, no section found */ }
		return section;
	}
	
	/**
	 * calculates a range of mentioned articles or paragraphs.
	 * 
	 * @param string the articles 
	 * @return List of related articles
	 */
	public static ArrayList<String> getNumbers(String string){
		ArrayList<String> paraList = new ArrayList<String>();
		String[] paragraph = {""};
		// the alphabet to find the bounds
		char[] alphabet = 	{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
							 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
		
			/* if the string relates to different paragraphs */
		if(containsRegex(".*Abs.*", string)){
			String[] abs = string.split("Abs");
		}else if(!containsRegex(".*Nr\\..*", string)){ 	/* find enumerations of articles or paragraphs */
			paragraph = string.split("( und )|(, )|( oder )"); 
		} else{
			paraList.add(getInteger(string.toCharArray(), false));
		}
		
		if(paragraph.length > 0){
			for(int j=0; j<paragraph.length; j++){
				
					/* 
					 * if the String contains "bis", the whole number of paragraphs
					 * between the upper and lower bound should be listed in the array
					 */
				if(containsRegex(".*bis.*", paragraph[j])){
					String[] diff = paragraph[j].split(" bis ");
					int upperInt = 0; int lowerInt = 0;
					try{
						lowerInt = Integer.parseInt(getInteger(diff[0].toCharArray(), true));
						upperInt = Integer.parseInt(getInteger(diff[1].toCharArray(), true));
						
							/* if the upper and lower bound only contain integers */
						for(int c=lowerInt; c<=upperInt; c++){ paraList.add(String.valueOf(c)); }
					} catch (NumberFormatException n){
						try{
							String lowerString = getInteger(diff[0].toCharArray(), true);
							String upperString = getInteger(diff[1].toCharArray(), true);
							int lowerBound = 0; int upperBound = 0;
							
							if(Integer.parseInt(getInteger(diff[0].toCharArray(), false)) < 
									Integer.parseInt(getInteger(diff[1].toCharArray(), false))){
								/* 
								 * if the upper or lower bound don't contain only integers
								 * and has got a higher range
								 */
								lowerInt = Integer.parseInt(getInteger(diff[0].toCharArray(), false));
								upperInt = Integer.parseInt(getInteger(diff[1].toCharArray(), false));	
								for(int c=lowerInt; c<=upperInt; c++){ paraList.add(String.valueOf(c)); }
								char upperAlphabet = upperString.toCharArray()[upperString.length()-1];
								for(int c = 0; c<alphabet.length; c++){
									
										// search for the matching upper bound
									if(alphabet[c] == upperAlphabet){ upperBound = c; }
								}
								for(int i=0; i<=upperBound; i++){ paraList.add(String.valueOf(upperInt)+alphabet[i]); }
							} else {
								
								/* if the lower bound contains only integers */
								if(lowerInt != 0){
									paraList.add(String.valueOf(lowerInt));
									char upperAlphabet = upperString.toCharArray()[upperString.length()-1];
									for(int c = 0; c<alphabet.length; c++){
										
											// search for the matching upper bound
										if(alphabet[c] == upperAlphabet){ upperBound = c; }
									}
										/* if both bounds don't contain only integers */
								} else {
									char upperAlphabet =  upperString.toCharArray()[upperString.length()-1];
									char lowerAlphabet =  lowerString.toCharArray()[lowerString.length()-1];
									for(int c = 0; c<alphabet.length; c++){
										
											// search for both matching bounds
										if(alphabet[c] == upperAlphabet){ upperBound = c; }
										if(alphabet[c] == lowerAlphabet){ lowerBound = c; }
									}
								}	// add all resulting articles to the paragraph list
								for(int c = lowerBound; c<=upperBound; c++){ paraList.add(getInteger(lowerString.toCharArray(), false)+alphabet[c]); }
							}
						} catch (IndexOutOfBoundsException i){
							if(lowerInt != 0){
								paraList.add(String.valueOf(lowerInt));
							}
						} catch (NumberFormatException n1){
							System.err.println((diff[0] + " " + diff[1]));
						}
					} catch (IndexOutOfBoundsException i){ 
						if(lowerInt != 0){
							paraList.add(String.valueOf(lowerInt));
						}
					}
				} else{
					if(containsRegex(".*[0-9]{1,3}.*", paragraph[j])){
						paraList.add(paragraph[j]);
					}
				}
			}
		} else { paraList.add(string); }
	return paraList;
	}
	
}
