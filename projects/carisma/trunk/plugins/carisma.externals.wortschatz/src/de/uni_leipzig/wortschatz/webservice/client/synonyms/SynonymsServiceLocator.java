/**
 * SynonymsServiceLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.synonyms;

public class SynonymsServiceLocator extends org.apache.axis.client.Service implements SynonymsService {

	public SynonymsServiceLocator() {
	}

	public SynonymsServiceLocator(final org.apache.axis.EngineConfiguration config) {
		super(config);
	}

	public SynonymsServiceLocator(final java.lang.String wsdlLoc, final javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
		super(wsdlLoc, sName);
	}

	// Use to get a proxy class for Synonyms
	private java.lang.String synonymsAddress = "http://pcai055.informatik.uni-leipzig.de:8100/axis/services/Synonyms";

	@Override
	public final java.lang.String getSynonymsAddress() {
		return synonymsAddress;
	}

	// The WSDD service name defaults to the port name.
	private java.lang.String synonymsWSDDServiceName = "Synonyms";

	public final java.lang.String getSynonymsWSDDServiceName() {
		return synonymsWSDDServiceName;
	}

	public final void setSynonymsWSDDServiceName(final java.lang.String name) {
		synonymsWSDDServiceName = name;
	}

	@Override
	public final Synonyms getSynonyms() throws javax.xml.rpc.ServiceException {
		java.net.URL endpoint;
		try {
			endpoint = new java.net.URL(synonymsAddress);
		} catch (java.net.MalformedURLException e) {
			throw new javax.xml.rpc.ServiceException(e);
		}
		return getSynonyms(endpoint);
	}

	@Override
	public final Synonyms getSynonyms(final java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
		try {
			SynonymsSoapBindingStub stub = new SynonymsSoapBindingStub(portAddress, this);
			stub.setPortName(getSynonymsWSDDServiceName());
			return stub;
		} catch (org.apache.axis.AxisFault e) {
			return null;
		}
	}

	public final void setSynonymsEndpointAddress(final java.lang.String address) {
		synonymsAddress = address;
	}

	/**
	 * For the given interface, get the stub implementation. If this service has
	 * no port for the given interface, then ServiceException is thrown.
	 */
	@Override
	public final java.rmi.Remote getPort(final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
		try {
			if (Synonyms.class.isAssignableFrom(serviceEndpointInterface)) {
				SynonymsSoapBindingStub stub = new SynonymsSoapBindingStub(new java.net.URL(synonymsAddress), this);
				stub.setPortName(getSynonymsWSDDServiceName());
				return stub;
			}
		} catch (java.lang.Throwable t) {
			throw new javax.xml.rpc.ServiceException(t);
		}
		throw new javax.xml.rpc.ServiceException("There is no stub implementation for the interface:  " + (serviceEndpointInterface == null ? "null" : serviceEndpointInterface.getName()));
	}

	/**
	 * For the given interface, get the stub implementation. If this service has
	 * no port for the given interface, then ServiceException is thrown.
	 */
	@Override
	public final java.rmi.Remote getPort(final javax.xml.namespace.QName portName, final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
		if (portName == null) {
			return getPort(serviceEndpointInterface);
		}
		java.lang.String inputPortName = portName.getLocalPart();
		if ("Synonyms".equals(inputPortName)) {
			return getSynonyms();
		} else {
			java.rmi.Remote stub = getPort(serviceEndpointInterface);
			((org.apache.axis.client.Stub) stub).setPortName(portName);
			return stub;
		}
	}

	@Override
	public final javax.xml.namespace.QName getServiceName() {
		return new javax.xml.namespace.QName("urn:Synonyms", "SynonymsService");
	}

	private java.util.HashSet ports = null;

	@Override
	public final java.util.Iterator getPorts() {
		if (ports == null) {
			ports = new java.util.HashSet();
			ports.add(new javax.xml.namespace.QName("urn:Synonyms", "Synonyms"));
		}
		return ports.iterator();
	}

	/**
	 * Set the endpoint address for the specified port name.
	 */
	public final void setEndpointAddress(final java.lang.String portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {
		if ("Synonyms".equals(portName)) {
			setSynonymsEndpointAddress(address);
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
