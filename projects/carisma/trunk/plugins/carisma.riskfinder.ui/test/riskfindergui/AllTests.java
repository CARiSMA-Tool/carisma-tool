package riskfindergui;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({ Bpmn2PainterTest.class, ModelControllerTest.class,
		RelevantWordsTest.class, RiskActivityTest.class, RiskPatternTest.class })
public class AllTests {

}
