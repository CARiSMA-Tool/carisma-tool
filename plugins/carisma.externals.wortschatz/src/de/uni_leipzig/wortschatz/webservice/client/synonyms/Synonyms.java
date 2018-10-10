/**
 * Synonyms.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.synonyms;

import de.uni_leipzig.wortschatz.webservice.client.utilities.RequestParameter;
import de.uni_leipzig.wortschatz.webservice.client.utilities.ResponseParameter;

public interface Synonyms extends java.rmi.Remote {
	public ResponseParameter execute(RequestParameter objRequestParameters) throws java.rmi.RemoteException;

	public java.lang.String ping() throws java.rmi.RemoteException;
}
