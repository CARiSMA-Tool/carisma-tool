package carisma.xutils.regulatory.extractor;

import java.io.File;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public interface RegulatoryExtractor {
	public RegulatoryOntologyHelper extract(final File sourceFolder);
	
	public void extract(final File sourceFolder, final RegulatoryOntologyHelper ontologyContainer);
}
