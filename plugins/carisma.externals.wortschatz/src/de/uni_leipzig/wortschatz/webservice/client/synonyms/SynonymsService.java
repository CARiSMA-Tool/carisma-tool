/**
 * SynonymsService.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.synonyms;

import javax.xml.rpc.Service;
import javax.xml.rpc.ServiceException;

public interface SynonymsService extends Service {
	public java.lang.String getSynonymsAddress();

	public Synonyms getSynonyms() throws ServiceException;

	public Synonyms getSynonyms(java.net.URL portAddress) throws ServiceException;
}
