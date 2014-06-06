package carisma.check.umlchange.efficiency;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;

public class TestTest {
	
	public static void main(String[] args) {
		LockedStatusModelCreater mc = new LockedStatusModelCreater();
		Model model = (Model) mc.getNewModel("TestModel", 100, (float) 10).getContents().get(0);
		System.out.println("Success");
		
	}

}
