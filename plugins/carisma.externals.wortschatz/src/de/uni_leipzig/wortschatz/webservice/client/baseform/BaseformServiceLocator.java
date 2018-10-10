/**
 * BaseformServiceLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.baseform;

public class BaseformServiceLocator extends org.apache.axis.client.Service implements de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformService {

    public BaseformServiceLocator() {
    }


    public BaseformServiceLocator(final org.apache.axis.EngineConfiguration config) {
        super(config);
    }

    public BaseformServiceLocator(final java.lang.String wsdlLoc, final javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
        super(wsdlLoc, sName);
    }

    // Use to get a proxy class for Baseform
    private java.lang.String baseformAddress = "http://pcai055.informatik.uni-leipzig.de:8100/axis/services/Baseform";

    public final java.lang.String getBaseformAddress() {
        return baseformAddress;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String baseformWSDDServiceName = "Baseform";

    public final java.lang.String getBaseformWSDDServiceName() {
        return baseformWSDDServiceName;
    }

    public final void setBaseformWSDDServiceName(final java.lang.String name) {
        baseformWSDDServiceName = name;
    }

    public final de.uni_leipzig.wortschatz.webservice.client.baseform.Baseform getBaseform() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
		try {
			endpoint = new java.net.URL(baseformAddress);
		} catch (java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getBaseform(endpoint);
    }

    public final de.uni_leipzig.wortschatz.webservice.client.baseform.Baseform getBaseform(final java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformSoapBindingStub stub = new de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformSoapBindingStub(portAddress, this);
            stub.setPortName(getBaseformWSDDServiceName());
            return stub;
		} catch (org.apache.axis.AxisFault e) {
			return null;
		}
    }

    public final void setBaseformEndpointAddress(final java.lang.String address) {
        baseformAddress = address;
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public final java.rmi.Remote getPort(final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        try {
            if (de.uni_leipzig.wortschatz.webservice.client.baseform.Baseform.class.isAssignableFrom(serviceEndpointInterface)) {
                de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformSoapBindingStub stub = new de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformSoapBindingStub(new java.net.URL(baseformAddress), this);
                stub.setPortName(getBaseformWSDDServiceName());
                return stub;
            }
        } catch (java.lang.Throwable t) {
            throw new javax.xml.rpc.ServiceException(t);
        }
        throw new javax.xml.rpc.ServiceException("There is no stub implementation for the interface:  " + (serviceEndpointInterface == null ? "null" : serviceEndpointInterface.getName()));
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public final java.rmi.Remote getPort(final javax.xml.namespace.QName portName, final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        if (portName == null) {
            return getPort(serviceEndpointInterface);
        }
        java.lang.String inputPortName = portName.getLocalPart();
        if ("Baseform".equals(inputPortName)) {
            return getBaseform();
        } else  {
            java.rmi.Remote stub = getPort(serviceEndpointInterface);
            ((org.apache.axis.client.Stub) stub).setPortName(portName);
            return stub;
        }
    }

    public final javax.xml.namespace.QName getServiceName() {
        return new javax.xml.namespace.QName("urn:Baseform", "BaseformService");
    }

    private java.util.HashSet ports = null;

    public final java.util.Iterator getPorts() {
        if (ports == null) {
            ports = new java.util.HashSet();
            ports.add(new javax.xml.namespace.QName("urn:Baseform", "Baseform"));
        }
        return ports.iterator();
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public final void setEndpointAddress(final java.lang.String portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {
        if ("Baseform".equals(portName)) {
            setBaseformEndpointAddress(address);
        } else { // Unknown Port Name
            throw new javax.xml.rpc.ServiceException(" Cannot set Endpoint Address for Unknown Port" + portName);
        }
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public final void setEndpointAddress(final javax.xml.namespace.QName portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {
        setEndpointAddress(portName.getLocalPart(), address);
    }

}
