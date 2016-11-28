package carisma.ocl;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.query.statements.IQueryResult;

/**
 * This class represents one QueryResult.
 * @author Marcel Michel
 *
 */
public class OclQueryResult {
	
	/**
	 * Constructor which creates one QueryResult object.
	 * 
	 * @param context The OCL-Context
	 * @param statement The OCL-Statement
	 * @param iQueryResult The OCL-Query result
	 */
	public OclQueryResult(EClass context, String statement, IQueryResult iQueryResult) {
		if (iQueryResult==null) {
			throw new IllegalArgumentException("IQueryResult must not be null!");
		}
		this.context = context;
		this.statement = statement;
		this.elements = iQueryResult;
	}
	
	/**
	 * The OCL-Context represented as String.
	 */
	private EClass context;
	
	/**
	 * The OCL-Statement represented as String.
	 */
	private String statement;
	
	/**
	 * The OCL-Query result represented as IQueryResult.
	 */
	private IQueryResult elements;
	
	public IQueryResult getElements() {
        return this.elements;
    }

    /**
	 * If at least one element is in the result set,
	 * then the constraint is violated.
	 * 
	 * @return Returns true if the constraint is violated otherwise false
	 */
	public boolean isViolated() {
		return this.elements != null && 
				this.elements.size() > 0;
	}
	
	/**
	 * Checks containment of an Object. 
	 * 
	 * @param o An Object, which should be checked
	 * @return Returns true if the QuereyResult contains Object o otherwise false 
	 */
	public boolean contains(Object o) {
		return this.elements.contains(o);
	}

	/**
	 * Checks containment of a Collection.
	 * 
	 * @param c A generic Collection, which should be checked
	 * @return Returns true if the QuereyResult contains Collection c otherwise false
	 */
	public boolean containsAll(Collection<?> c) {
		return this.elements.containsAll(c);
	}

	/**
	 * A getter for EObjects.
	 * 
	 * @return Returns set of the EObjects
	 */
	public Set<? extends EObject> getEObjects() {
		return this.elements.getEObjects();
	}

	/**
	 * A getter for the exception, which maybe occurred during the query.
	 * 
	 * @return Returns the exception of the query
	 */
	public Exception getException() {
		return this.elements.getException();
	}

	/**
	 * A getter for the iterator.
	 * 
	 * @return Returns the iterator of the QueryResult
	 */
	public Iterator<EObject> iterator() {
		return this.elements.iterator();
	}

	/**
	 * A getter for the size of the QueryResult.
	 * 
	 * @return Returns size of the QueryResult
	 */
	public int size() {
		return this.elements.size();
	}
	
	/**
	 * A getter for the ocl context class, represented as an EObject.
	 * 
	 * @return Returns EObject class of the context
	 */
	public EClass getContext() {
		return this.context;
	}

	/**
	 *A getter for the ocl context class, represented as a string object.
	 * 
	 * @return If the context is null, the context free string
	 * was used during the query and will be returned otherwise the
	 * name of the EObject
	 */
	public String getContextString() {
		return (this.context == null) ? OclEvaluator.CONTEXT_FREE : this.context.getName();
	}
	
	/**
	 * A getter for the ocl statement.
	 * 
	 * @return Returns ocl statement
	 */
	public String getStatement() {
		return this.statement;
	}
}
