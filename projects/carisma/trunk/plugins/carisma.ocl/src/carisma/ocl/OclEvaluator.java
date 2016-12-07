package carisma.ocl;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.query.conditions.eobjects.EObjectCondition;
import org.eclipse.emf.query.ocl.conditions.BooleanOCLCondition;
import org.eclipse.emf.query.statements.FROM;
import org.eclipse.emf.query.statements.IQueryResult;
import org.eclipse.emf.query.statements.SELECT;
import org.eclipse.emf.query.statements.WHERE;
import org.eclipse.ocl.ParserException;
import org.eclipse.ocl.ecore.OCL;

/**
 * This class provides a static OclQuery Method.
 * @author Marcel Michel
 *
 */
public final class OclEvaluator {

    /** Hide constructor
     */
    private OclEvaluator() {
    }
    
	/**
	 * String used to identify context free queries.  
	 */
	public static final String CONTEXT_FREE = "context-free";
	
	/**
	 * Query a model with a given OCL expression represented as 
	 * statement and context.
	 * 
	 * @param root The root element of the model
	 * @param oclStatement The OCL statement
	 * @param oclContext The OCL context
	 * @return returns an IQueryResult with all model elements 
	 * 			which fits to the oclExpression
	 * @throws ParserException If the OCL Expression could not be executed
	 */
	public static OclQueryResult query(final EObject root, final EClass oclContext, 
			final String oclStatement) throws ParserException {
		OCL ocl = org.eclipse.ocl.ecore.OCL.newInstance();

		EObjectCondition condition = new BooleanOCLCondition<>(
                ocl.getEnvironment(),
                "not(" + oclStatement + ")",
                oclContext);
        SELECT statement = new SELECT(SELECT.UNBOUNDED, false, new FROM(root),
                new WHERE(condition), new NullProgressMonitor());

        IQueryResult result = statement.execute();
		
        return new OclQueryResult(oclContext, oclStatement, result);
	}
	
}
