/**
 * RequestParameter.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.utilities;

public class RequestParameter  implements java.io.Serializable {
    private java.lang.String corpus;
    private DataMatrix parameters;

    public RequestParameter() {
    }

    public RequestParameter(
    		final java.lang.String corpus, final DataMatrix parameters) {
           this.corpus = corpus;
           this.parameters = parameters;
    }


    /**
     * Gets the corpus value for this RequestParameter.
     * 
     * @return corpus
     */
    public final java.lang.String getCorpus() {
        return corpus;
    }


    /**
     * Sets the corpus value for this RequestParameter.
     * 
     * @param corpus
     */
    public final void setCorpus(final java.lang.String corpus) {
        this.corpus = corpus;
    }


    /**
     * Gets the parameters value for this RequestParameter.
     * 
     * @return parameters
     */
    public final DataMatrix getParameters() {
        return parameters;
    }


    /**
     * Sets the parameters value for this RequestParameter.
     * 
     * @param parameters
     */
    public final void setParameters(final DataMatrix parameters) {
        this.parameters = parameters;
    }

    private java.lang.Object equalsCalc = null;
    public final synchronized boolean equals(final java.lang.Object obj) {
        if (!(obj instanceof RequestParameter)) { return false; }
        RequestParameter other = (RequestParameter) obj;
        /* dead code: if (obj == null) { return false; } */
        if (this == obj) { return true; }
        if (equalsCalc != null) {
            return (equalsCalc == obj);
        }
        equalsCalc = obj;
        boolean equals;
        equals = 
            (
            		(this.corpus == null && other.getCorpus() == null) 
            		|| 
            		(this.corpus != null && this.corpus.equals(other.getCorpus()))
            )            		
             && 
             (
            		 (this.parameters == null && other.getParameters() == null) 
            		 ||
            		 (this.parameters != null && this.parameters.equals(other.getParameters()))
            );
        equalsCalc = null;
        return equals;
    }

    private boolean hashCodeCalc = false;
    public final synchronized int hashCode() {
        if (hashCodeCalc) {
            return 0;
        }
        hashCodeCalc = true;
        int hashCode = 1;
        if (getCorpus() != null) {
            hashCode += getCorpus().hashCode();
        }
        if (getParameters() != null) {
            hashCode += getParameters().hashCode();
        }
        hashCodeCalc = false;
        return hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(RequestParameter.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("urn:Synonyms", "RequestParameter"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("corpus");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "corpus"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setNillable(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("parameters");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "parameters"));
        elemField.setXmlType(new javax.xml.namespace.QName("urn:Synonyms", "DataMatrix"));
        elemField.setNillable(true);
        typeDesc.addFieldDesc(elemField);
    }

    /**
     * Return type metadata object.
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

    /**
     * Get Custom Serializer.
     */
    public static org.apache.axis.encoding.Serializer getSerializer(
    		final java.lang.String mechType, 
           final java.lang.Class javaType,  
           final javax.xml.namespace.QName xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanSerializer(
            javaType, xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer.
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
    		final java.lang.String mechType, 
    		final java.lang.Class javaType,  
    		final javax.xml.namespace.QName xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            javaType, xmlType, typeDesc);
    }

}
