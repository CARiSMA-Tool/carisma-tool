package carisma.check.sequencediagramcrypto.replacewithjava8;

import java.util.NoSuchElementException;

// TODO : Remove this class when switching to Java8!
public class Optional<T> {
	
	private final T	value;
	
	private Optional() {
		this.value = null;
	}
	
	private Optional(T value) {
		this.value = value;
	}
	
	public static <T> Optional<T> empty() {
		return new Optional<T>();
	}
	
	public static <T> Optional<T> of(T value) {
		return new Optional<T>(value);
	}
	
	public T get() {
		if (this.value == null) {
			throw new NoSuchElementException();
		}
		return this.value;
	}
	
	public boolean isPresent() {
		return this.value != null;
	}
	
}
