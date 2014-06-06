/**
 * CooccurrencesService.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.coocurrences;

public interface CooccurrencesService extends javax.xml.rpc.Service {
	public java.lang.String getCooccurrencesAddress();

	public Cooccurrences getCooccurrences() throws javax.xml.rpc.ServiceException;

	public Cooccurrences getCooccurrences(java.net.URL portAddress) throws javax.xml.rpc.ServiceException;
}
