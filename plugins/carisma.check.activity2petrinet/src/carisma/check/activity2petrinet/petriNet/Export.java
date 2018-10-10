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
package carisma.check.activity2petrinet.petriNet;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;


/**
 * This class exports a petri net object to a file in pnml format.
 * @author Kubi Mensah
 *
 */
public class Export {
	
	
	/**
	 *The to be exported petri net object. 
	 */
	private  PetriNet petriNet;
	
	/**
	 * Constucts an export object given the to be exported petri net object.
	 * @param petriNet a petr net object
	 */
	public Export(final PetriNet petriNet) {
		this.petriNet = petriNet;
	}

	/**
	 * Writes the petri net object to a file in pnml format.
	 * @param filePath the file path where the petri net should be exported to
	 * @return true if the petri net was exported successful 
	 */
	public final boolean exportToPNML(final String filePath) {
		return this.exportToPNML(filePath, true);
	}
	
	/**
	 * Writes the petri net object to a file in pnml format.
	 * @param filePath the file path where the petri net should be exported to
	 * @param withPage surround entries with page-tag
	 * @return true if the petri net was exported successful 
	 */
	public final boolean exportToPNML(final String filePath, final boolean withPage) {
		StringBuffer exportString = new StringBuffer("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
		exportString.append("<pnml xmlns=\"http://www.pnml.org/version-2009/grammar/pnml\">\n");
		
		exportString.append("<net id=\"");
		exportString.append(this.petriNet.getId());
		exportString.append("\" type=\"");
		exportString.append(this.petriNet.getType());
		exportString.append("\">\n");
		
		if (withPage) {
			exportString.append("<page id=\"page1-net1\">\n");			
		}
		
		for (Place place : this.petriNet.getPlaces()) {
		    exportString.append("\t<");
		    exportString.append(place.getType());
		    exportString.append(" id=\"");
		    exportString.append(place.getId());
		    exportString.append("\">\n");
			if (place.getGraphics() != null) {
			    exportString.append("\t\t<graphics>\n");
			    exportString.append("\t\t\t<position x=\"");
			    exportString.append(place.getGraphics().getPositionX());
			    exportString.append("\" y=\"");
			    exportString.append(place.getGraphics().getPositionY());
			    exportString.append("\" />\n");
			    exportString.append("\t\t\t<dimension x=\"");
			    exportString.append(place.getGraphics().getDimensionX());
			    exportString.append("\" y=\"");
			    exportString.append(place.getGraphics().getDimensionY());
			    exportString.append("\" />\n");
			    exportString.append("\t\t</graphics>\n");
			}
			exportString.append("\t\t<name>\n");
			exportString.append("\t\t\t<text>");
			exportString.append(place.getName());
			exportString.append("</text>\n");
			exportString.append("\t\t</name>\n");
			// initial Markings, if set
			if (place.getInitialMarking() > 0) {
				exportString.append("\t\t<initialMarking>\n");
				exportString.append("\t\t\t<text>");
				exportString.append(place.getInitialMarking());
				exportString.append("</text>\n");
				exportString.append("\t\t</initialMarking>\n");
			}
			exportString.append("\t</");
			exportString.append(place.getType());
			exportString.append(">\n");
		}
		
		for (Transition transition : this.petriNet.getTransitions()) {
		    exportString.append("\t<");
		    exportString.append(transition.getType());
		    exportString.append(" id=\"");
		    exportString.append(transition.getId());
		    exportString.append("\">\n");
			if (transition.getGraphics() != null) {
			    exportString.append("\t\t<graphics>\n");
			    exportString.append("\t\t\t<position x=\"");
			    exportString.append(transition.getGraphics().getPositionX()); 
			    exportString.append("\" y=\"");
			    exportString.append(transition.getGraphics().getPositionY());
			    exportString.append("\" />\n");
			    exportString.append("\t\t\t<dimension x=\"");
			    exportString.append(transition.getGraphics().getDimensionX()); 
			    exportString.append("\" y=\"");
				exportString.append(transition.getGraphics().getDimensionY());
				exportString.append("\" />\n");
				exportString.append("\t\t</graphics>\n");
			}
			exportString.append("\t\t<name>\n");
			exportString.append("\t\t\t<text>");
			exportString.append(transition.getName());
			exportString.append("</text>\n");
			exportString.append("\t\t</name>\n");
			exportString.append("\t\t<toolspecific tool= \"ProM\" version=\"\">\n");
			exportString.append("\t\t\t<logevent>\n");
			exportString.append("\t\t\t\t<name>" + transition.getName() + "</name>\n");
			exportString.append("\t\t\t</logevent>\n");
			exportString.append("\t\t</toolspecific>\n");
			exportString.append("\t</");
			exportString.append(transition.getType());
			exportString.append(">\n");
		}
		
		for (Arc arc : this.petriNet.getArcs()) {
		    exportString.append("\t<");
		    exportString.append(arc.getType());
		    exportString.append(" id=\"");
		    exportString.append(arc.getId());
		    exportString.append("\" source=\"");
		    exportString.append(arc.getSource());
		    exportString.append("\" target=\"");
		    exportString.append(arc.getTarget());
		    exportString.append("\">\n");
		    exportString.append("\t</");
		    exportString.append(arc.getType());
		    exportString.append(">\n");
		}
		
		if (withPage) {
			exportString.append("</page>");			
		}
		
		exportString.append("</net>\n"); 
		exportString.append("</pnml>\n");
		
		try(BufferedWriter out = new BufferedWriter(new FileWriter(filePath))){
			out.write(exportString.toString());
			return true;
		} catch (IOException e) {
			return false;
		}
	}
}
