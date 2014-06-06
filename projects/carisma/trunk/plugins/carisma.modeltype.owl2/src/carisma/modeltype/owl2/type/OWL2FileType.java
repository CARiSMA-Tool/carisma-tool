/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.modeltype.owl2.type;

import org.coode.owl.krssparser.KRSSOntologyFormat;
import org.coode.owlapi.latex.LatexAxiomsListOntologyFormat;
import org.coode.owlapi.latex.LatexOntologyFormat;
import org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat;
import org.coode.owlapi.obo.parser.OBOOntologyFormat;
import org.coode.owlapi.turtle.TurtleOntologyFormat;
import org.semanticweb.owlapi.io.OWLFunctionalSyntaxOntologyFormat;
import org.semanticweb.owlapi.io.OWLXMLOntologyFormat;
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyFormat;

import uk.ac.manchester.cs.owlapi.dlsyntax.DLSyntaxHTMLOntologyFormat;
import uk.ac.manchester.cs.owlapi.dlsyntax.DLSyntaxOntologyFormat;
import uk.ac.manchester.owl.owlapi.tutorialowled2011.OWLTutorialSyntaxOntologyFormat;
import de.uulm.ecs.ai.owlapi.krssparser.KRSS2OntologyFormat;

/**
 * Lists all available file types.
 * 
 * @author Marcel Michel
 */
public enum OWL2FileType {
	
	DL_SYNTAX_HTML(
			new DLSyntaxHTMLOntologyFormat()),
	DL_SYNTAX(
			new DLSyntaxOntologyFormat()),
	KRSS2(
			new KRSS2OntologyFormat()),
	KRSS(
			new KRSSOntologyFormat()),
	LATEX_AXIOMSLIST(
			new LatexAxiomsListOntologyFormat()),
	LATEX(
			new LatexOntologyFormat()),
	OBO(
			new OBOOntologyFormat()),
	MANCHESTER_OWL_SYNTAX(
			new ManchesterOWLSyntaxOntologyFormat()),
	OWL_FUNCTIONAL_SYNTAX(
			new OWLFunctionalSyntaxOntologyFormat()),
	OWL_TUTORIAL_SYNTAX(
			new OWLTutorialSyntaxOntologyFormat()),
	OWL_XML(
			new OWLXMLOntologyFormat()),
	RDF_XML(
			new RDFXMLOntologyFormat()),
	TURTLE(
			new TurtleOntologyFormat());
	
	private OWLOntologyFormat owlOntologyFormat;
	
	private OWL2FileType(OWLOntologyFormat owlOntologyFormat) {
		this.owlOntologyFormat = owlOntologyFormat;
	}
	
	public OWLOntologyFormat getOWLOntologyFormat() {
		return this.owlOntologyFormat;
	}
}
