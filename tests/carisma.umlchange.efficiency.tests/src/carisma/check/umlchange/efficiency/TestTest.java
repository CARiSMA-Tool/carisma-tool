package carisma.check.umlchange.efficiency;

import org.eclipse.uml2.uml.Model;
import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class TestTest {
	
	@Test
	public void test() {
		LockedStatusModelCreater mc = new LockedStatusModelCreater();
		@SuppressWarnings("unused")
		Model model = (Model) mc.getNewModel("TestModel", 100, 10).getContents().get(0);
		System.out.println("Success");
		
	}

}
