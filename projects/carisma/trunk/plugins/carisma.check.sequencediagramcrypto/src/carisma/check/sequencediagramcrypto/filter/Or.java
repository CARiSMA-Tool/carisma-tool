package carisma.check.sequencediagramcrypto.filter;


public class Or<T> implements Filter<T> {
	
	final private Filter<T>	filter1;
	final private Filter<T>	filter2;
	
	public Or(final Filter<T> filter1, final Filter<T> filter2) {
		this.filter1 = filter1;
		this.filter2 = filter2;
	}
	
	@Override
	public boolean accept(T o) {
		return this.filter1.accept(o) || this.filter2.accept(o);
	}
	
}
