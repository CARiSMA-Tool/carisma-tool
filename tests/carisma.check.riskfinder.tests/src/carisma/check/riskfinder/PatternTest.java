//package carisma.check.riskfinder;
//
////import static org.junit.Assert.*;
//
//import static org.junit.Assert.assertEquals;
//
//import java.util.ArrayList;
//import org.junit.Test;
//
//import carisma.check.riskfinder.Pattern;
//import carisma.check.riskfinder.StopwordFilter;
//import carisma.check.riskfinder.Pattern.PatternTextType;
//
//
//public class PatternTest {
//
//	@Test
//	public final void test() {
//		Pattern testPattern = new Pattern("asdf", "title342", "123");
//		String output = testPattern.toString();
//		System.out.println(output);
//		assertEquals(output, "(asdf : title342 : 123)");
//		
//		testPattern = new Pattern("sadf", "klsdfjls", "abc def 123.dkfj, zzz");
//		System.out.println(testPattern.getText());
//		ArrayList<String> split = testPattern.getWords(PatternTextType.TEXT);
//		for (String curWord : split) {
//			System.out.println(curWord);
//		}
//		System.out.println(split.toString());
//		System.out.println(split.size());
//		assertEquals(4, split.size());
//		assertEquals("abc", split.get(0));
//		assertEquals("def", split.get(1));
//		assertEquals("123.dkfj,", split.get(2));
//		assertEquals("zzz", split.get(3));
//		
//		testPattern = new Pattern("Test f�r Stopwords", "Mit Titel", "Alles ist gut!");
//		System.out.println(testPattern.getText());
//		
//		StopwordFilter stopwords = new StopwordFilter();
//		stopwords.add("ist");
//		
////	 	Set<String> split2=testPattern.getWordsSetNoStopwords(PatternTextType.TEXT,stopwords);
////		for (String curWord : split2)
////			System.out.println(curWord);
////		System.out.println (split2.toString());		
////		System.out.println (split2.size());
////		assertEquals(2, split2.size());
////		assert(
////				(split.get(0).equals("Alles") && split.get(1).equals("gut"))
////				||
////				(split.get(1).equals("Alles") && split.get(0).equals("gut"))
////				);
//			}
//	
//		
//
//}
