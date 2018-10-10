/**
 * BaseformService.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.baseform;

public interface BaseformService extends javax.xml.rpc.Service {
    public java.lang.String getBaseformAddress();

    public de.uni_leipzig.wortschatz.webservice.client.baseform.Baseform getBaseform() throws javax.xml.rpc.ServiceException;

    public de.uni_leipzig.wortschatz.webservice.client.baseform.Baseform getBaseform(java.net.URL portAddress) throws javax.xml.rpc.ServiceException;
}
