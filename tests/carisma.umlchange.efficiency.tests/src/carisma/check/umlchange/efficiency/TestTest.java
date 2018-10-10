package carisma.check.umlchange.efficiency;

import org.eclipse.uml2.uml.Model;

public class TestTest {
	
	//TODO: Has to be executed as plugin, convert to JUnit Plugin Test?
	public static void main(String[] args) {
		LockedStatusModelCreater mc = new LockedStatusModelCreater();
		Model model = (Model) mc.getNewModel("TestModel", 100, 10).getContents().get(0);
		System.out.println("Success");
		
	}

}
