package carisma.check.riskfinder;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import carisma.check.riskfinder.StopwordFilter;


public class StopwordCollectionTest {

	@Test
	public final void test() {
		// out.println(System.getProperty("user.dir"));
		StopwordFilter words = new StopwordFilter();
		words.loadFromFile("resources/junit_stopwords.txt");

		System.out.println(words);

		assertEquals("[jkl, lksdjfklsjlfkdjl, sdkls k lksjdf, lsdjfklsj]",
				words.toString());
	}

}
