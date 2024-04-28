package carisma.check.rabac;

import org.eclipse.emf.ecore.resource.Resource;

@SuppressWarnings("java:S5960")
public interface RABACTest {

	Resource getModel();

	StringBuilder getReport();

}
