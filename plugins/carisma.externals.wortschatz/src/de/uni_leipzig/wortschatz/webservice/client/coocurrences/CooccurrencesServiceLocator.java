/**
 * CooccurrencesServiceLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.coocurrences;

public class CooccurrencesServiceLocator extends org.apache.axis.client.Service implements CooccurrencesService {

	public CooccurrencesServiceLocator() {
	}

	public CooccurrencesServiceLocator(final org.apache.axis.EngineConfiguration config) {
		super(config);
	}

	public CooccurrencesServiceLocator(final java.lang.String wsdlLoc, final javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
		super(wsdlLoc, sName);
	}

	// Use to get a proxy class for Cooccurrences
	private java.lang.String cooccurrencesAddress = "http://pcai055.informatik.uni-leipzig.de:8100/axis/services/Cooccurrences";

	@Override
	public final java.lang.String getCooccurrencesAddress() {
		return cooccurrencesAddress;
	}

	// The WSDD service name defaults to the port name.
	private java.lang.String cooccurrencesWSDDServiceName = "Cooccurrences";

	public final java.lang.String getCooccurrencesWSDDServiceName() {
		return cooccurrencesWSDDServiceName;
	}

	public final void setCooccurrencesWSDDServiceName(final java.lang.String name) {
		cooccurrencesWSDDServiceName = name;
	}

	@Override
	public final Cooccurrences getCooccurrences() throws javax.xml.rpc.ServiceException {
		java.net.URL endpoint;
		try {
			endpoint = new java.net.URL(cooccurrencesAddress);
		} catch (java.net.MalformedURLException e) {
			throw new javax.xml.rpc.ServiceException(e);
		}
		return getCooccurrences(endpoint);
	}

	@Override
	public final Cooccurrences getCooccurrences(final java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
		try {
			CooccurrencesSoapBindingStub stub = new CooccurrencesSoapBindingStub(portAddress, this);
			stub.setPortName(getCooccurrencesWSDDServiceName());
			return stub;
		} catch (org.apache.axis.AxisFault e) {
			return null;
		}
	}

	public final void setCooccurrencesEndpointAddress(final java.lang.String address) {
		cooccurrencesAddress = address;
	}

	/**
	 * For the given interface, get the stub implementation. If this service has
	 * no port for the given interface, then ServiceException is thrown.
	 */
	@Override
	public final java.rmi.Remote getPort(final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
		try {
			if (Cooccurrences.class.isAssignableFrom(serviceEndpointInterface)) {
				CooccurrencesSoapBindingStub stub = new CooccurrencesSoapBindingStub(new java.net.URL(cooccurrencesAddress), this);
				stub.setPortName(getCooccurrencesWSDDServiceName());
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
		if ("Cooccurrences".equals(inputPortName)) {
			return getCooccurrences();
		} else {
			java.rmi.Remote stub = getPort(serviceEndpointInterface);
			((org.apache.axis.client.Stub) stub).setPortName(portName);
			return stub;
		}
	}

	@Override
	public final javax.xml.namespace.QName getServiceName() {
		return new javax.xml.namespace.QName("urn:Cooccurrences", "CooccurrencesService");
	}

	private java.util.HashSet ports = null;

	@Override
	public final java.util.Iterator getPorts() {
		if (ports == null) {
			ports = new java.util.HashSet();
			ports.add(new javax.xml.namespace.QName("urn:Cooccurrences", "Cooccurrences"));
		}
		return ports.iterator();
	}

	/**
	 * Set the endpoint address for the specified port name.
	 */
	public final void setEndpointAddress(final java.lang.String portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {
		if ("Cooccurrences".equals(portName)) {
			setCooccurrencesEndpointAddress(address);
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
