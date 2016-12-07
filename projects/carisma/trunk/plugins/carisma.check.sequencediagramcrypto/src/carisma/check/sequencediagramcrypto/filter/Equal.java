package carisma.check.sequencediagramcrypto.filter;


public class Equal<T> implements Filter<T> {
	
	private final T	o;
	
	public Equal(final T o) {
		this.o = o;
	}
	
	@Override
	public boolean accept(T o) {
		if (this.o == null) {
			if (o == null) {
				return true;
			}
			return false;
		}
		return this.o.equals(o);
	}
	
}
