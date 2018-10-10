/**
 * SynonymsSoapBindingStub.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.synonyms;

import de.uni_leipzig.wortschatz.webservice.client.utilities.DataMatrix;
import de.uni_leipzig.wortschatz.webservice.client.utilities.DataVector;
import de.uni_leipzig.wortschatz.webservice.client.utilities.RequestParameter;
import de.uni_leipzig.wortschatz.webservice.client.utilities.ResponseParameter;

public class SynonymsSoapBindingStub extends org.apache.axis.client.Stub implements Synonyms {
	private final java.util.Vector cachedSerClasses = new java.util.Vector();
	private final java.util.Vector cachedSerQNames = new java.util.Vector();
	private final java.util.Vector cachedSerFactories = new java.util.Vector();
	private final java.util.Vector cachedDeserFactories = new java.util.Vector();

	static org.apache.axis.description.OperationDesc[] operations;

	static {
		operations = new org.apache.axis.description.OperationDesc[2];
		initOperationDesc1();
	}

	private static void initOperationDesc1() {
		org.apache.axis.description.OperationDesc oper;
		oper = new org.apache.axis.description.OperationDesc();
		oper.setName("execute");
		oper.addParameter(new javax.xml.namespace.QName("urn:Synonyms", "objRequestParameters"), new javax.xml.namespace.QName("urn:Synonyms", "RequestParameter"), RequestParameter.class, org.apache.axis.description.ParameterDesc.IN, false, false);
		oper.setReturnType(new javax.xml.namespace.QName("urn:Synonyms", "ResponseParameter"));
		oper.setReturnClass(ResponseParameter.class);
		oper.setReturnQName(new javax.xml.namespace.QName("urn:Synonyms", "executeReturn"));
		oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
		oper.setUse(org.apache.axis.constants.Use.LITERAL);
		operations[0] = oper;

		oper = new org.apache.axis.description.OperationDesc();
		oper.setName("ping");
		oper.setReturnType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
		oper.setReturnClass(java.lang.String.class);
		oper.setReturnQName(new javax.xml.namespace.QName("urn:Synonyms", "pingReturn"));
		oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
		oper.setUse(org.apache.axis.constants.Use.LITERAL);
		operations[1] = oper;

	}

	public SynonymsSoapBindingStub() throws org.apache.axis.AxisFault {
		this(null);
	}

	public SynonymsSoapBindingStub(final java.net.URL endpointURL, final javax.xml.rpc.Service service) throws org.apache.axis.AxisFault {
		this(service);
		super.cachedEndpoint = endpointURL;
	}

	public SynonymsSoapBindingStub(final javax.xml.rpc.Service service) throws org.apache.axis.AxisFault {
		if (service == null) {
			super.service = new org.apache.axis.client.Service();
		} else {
			super.service = service;
		}
		((org.apache.axis.client.Service) super.service).setTypeMappingVersion("1.2");
		java.lang.Class cls;
		javax.xml.namespace.QName qName;
		java.lang.Class beansf = org.apache.axis.encoding.ser.BeanSerializerFactory.class;
		java.lang.Class beandf = org.apache.axis.encoding.ser.BeanDeserializerFactory.class;
		java.lang.Class enumsf = org.apache.axis.encoding.ser.EnumSerializerFactory.class;
		java.lang.Class enumdf = org.apache.axis.encoding.ser.EnumDeserializerFactory.class;
		java.lang.Class arraysf = org.apache.axis.encoding.ser.ArraySerializerFactory.class;
		java.lang.Class arraydf = org.apache.axis.encoding.ser.ArrayDeserializerFactory.class;
		java.lang.Class simplesf = org.apache.axis.encoding.ser.SimpleSerializerFactory.class;
		java.lang.Class simpledf = org.apache.axis.encoding.ser.SimpleDeserializerFactory.class;
		java.lang.Class simplelistsf = org.apache.axis.encoding.ser.SimpleListSerializerFactory.class;
		java.lang.Class simplelistdf = org.apache.axis.encoding.ser.SimpleListDeserializerFactory.class;
		qName = new javax.xml.namespace.QName("urn:Synonyms", "ResponseParameter");
		cachedSerQNames.add(qName);
		cls = ResponseParameter.class;
		cachedSerClasses.add(cls);
		cachedSerFactories.add(beansf);
		cachedDeserFactories.add(beandf);

		qName = new javax.xml.namespace.QName("urn:Synonyms", "RequestParameter");
		cachedSerQNames.add(qName);
		cls = RequestParameter.class;
		cachedSerClasses.add(cls);
		cachedSerFactories.add(beansf);
		cachedDeserFactories.add(beandf);

		qName = new javax.xml.namespace.QName("urn:Synonyms", "DataMatrix");
		cachedSerQNames.add(qName);
		cls = DataMatrix.class;
		cachedSerClasses.add(cls);
		cachedSerFactories.add(beansf);
		cachedDeserFactories.add(beandf);

		qName = new javax.xml.namespace.QName("http://datatypes.webservice.wortschatz.uni_leipzig.de", "DataVector");
		cachedSerQNames.add(qName);
		cls = DataVector.class;
		cachedSerClasses.add(cls);
		cachedSerFactories.add(beansf);
		cachedDeserFactories.add(beandf);

	}

	protected final org.apache.axis.client.Call createCall() throws java.rmi.RemoteException {
		try {
			org.apache.axis.client.Call call = (org.apache.axis.client.Call) super.service.createCall();
			if (super.maintainSessionSet) {
				call.setMaintainSession(super.maintainSession);
			}
			if (super.cachedUsername != null) {
				call.setUsername(super.cachedUsername);
			}
			if (super.cachedPassword != null) {
				call.setPassword(super.cachedPassword);
			}
			if (super.cachedEndpoint != null) {
				call.setTargetEndpointAddress(super.cachedEndpoint);
			}
			if (super.cachedTimeout != null) {
				call.setTimeout(super.cachedTimeout);
			}
			if (super.cachedPortName != null) {
				call.setPortName(super.cachedPortName);
			}
			java.util.Enumeration keys = super.cachedProperties.keys();
			while (keys.hasMoreElements()) {
				java.lang.String key = (java.lang.String) keys.nextElement();
				call.setProperty(key, super.cachedProperties.get(key));
			}
			// All the type mapping information is registered
			// when the first call is made.
			// The type mapping information is actually registered in
			// the TypeMappingRegistry of the service, which
			// is the reason why registration is only needed for the first call.
			synchronized (this) {
				if (firstCall()) {
					// must set encoding style before registering serializers
					call.setEncodingStyle(null);
					for (int i = 0; i < cachedSerFactories.size(); ++i) {
						java.lang.Class cls = (java.lang.Class) cachedSerClasses.get(i);
						javax.xml.namespace.QName qName = (javax.xml.namespace.QName) cachedSerQNames.get(i);
						java.lang.Object x = cachedSerFactories.get(i);
						if (x instanceof Class) {
							java.lang.Class sf = (java.lang.Class) cachedSerFactories.get(i);
							java.lang.Class df = (java.lang.Class) cachedDeserFactories.get(i);
							call.registerTypeMapping(cls, qName, sf, df, false);
						} else if (x instanceof javax.xml.rpc.encoding.SerializerFactory) {
							org.apache.axis.encoding.SerializerFactory sf = (org.apache.axis.encoding.SerializerFactory) cachedSerFactories.get(i);
							org.apache.axis.encoding.DeserializerFactory df = (org.apache.axis.encoding.DeserializerFactory) cachedDeserFactories.get(i);
							call.registerTypeMapping(cls, qName, sf, df, false);
						}
					}
				}
			}
			return call;
		} catch (java.lang.Throwable throwable) {
			throw new org.apache.axis.AxisFault("Failure trying to get the Call object", throwable);
		}
	}

	@Override
	public final ResponseParameter execute(final RequestParameter objRequestParameters) throws java.rmi.RemoteException {
		if (super.cachedEndpoint == null) {
			throw new org.apache.axis.NoEndPointException();
		}
		org.apache.axis.client.Call call = createCall();
		call.setOperation(operations[0]);
		call.setUseSOAPAction(true);
		call.setSOAPActionURI("");
		call.setEncodingStyle(null);
		call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
		call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
		call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP11_CONSTANTS);
		call.setOperationName(new javax.xml.namespace.QName("urn:Synonyms", "execute"));

		setRequestHeaders(call);
		setAttachments(call);
		try {
			java.lang.Object resp = call.invoke(new java.lang.Object[] { objRequestParameters });

			if (resp instanceof java.rmi.RemoteException) {
				throw (java.rmi.RemoteException) resp;
			} else {
				extractAttachments(call);
				try {
					return (ResponseParameter) resp;
				} catch (java.lang.Exception exception) {
					return (ResponseParameter) org.apache.axis.utils.JavaUtils.convert(resp, ResponseParameter.class);
				}
			}
		} catch (org.apache.axis.AxisFault axisFaultException) {
			throw axisFaultException;
		}
	}

	@Override
	public final java.lang.String ping() throws java.rmi.RemoteException {
		if (super.cachedEndpoint == null) {
			throw new org.apache.axis.NoEndPointException();
		}
		org.apache.axis.client.Call call = createCall();
		call.setOperation(operations[1]);
		call.setUseSOAPAction(true);
		call.setSOAPActionURI("");
		call.setEncodingStyle(null);
		call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
		call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
		call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP11_CONSTANTS);
		call.setOperationName(new javax.xml.namespace.QName("urn:Synonyms", "ping"));

		setRequestHeaders(call);
		setAttachments(call);
		try {
			java.lang.Object resp = call.invoke(new java.lang.Object[] {});

			if (resp instanceof java.rmi.RemoteException) {
				throw (java.rmi.RemoteException) resp;
			} else {
				extractAttachments(call);
				try {
					return (java.lang.String) resp;
				} catch (java.lang.Exception exception) {
					return (java.lang.String) org.apache.axis.utils.JavaUtils.convert(resp, java.lang.String.class);
				}
			}
		} catch (org.apache.axis.AxisFault axisFaultException) {
			throw axisFaultException;
		}
	}

}
