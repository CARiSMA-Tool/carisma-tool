package carisma.check.processanalysis.texttools.wortschatz;

public class Testclass {

	public static void draw(int n){
		for(int z = 0; z <= n; z++, System.out.println(""))
			for(int s = 0; s <= n; s++, System.out.print((z*(s-1)*(z-n)*(s-n-1)==0)?"*":" "));
	}
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Testclass.draw(10);

	}

}
