/**
 * ResponseParameter.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.utilities;

public class ResponseParameter  implements java.io.Serializable {
    private java.lang.String executionTime;
    private DataMatrix result;
    private int serviceMagnitude;
    private int userAmount;
    private int userMaxLimit;

    public ResponseParameter() {
    }

    public ResponseParameter(
    		final java.lang.String executionTime,
    		final DataMatrix result,
    		final int serviceMagnitude,
    		final int userAmount,
    		final int userMaxLimit) {
           this.executionTime = executionTime;
           this.result = result;
           this.serviceMagnitude = serviceMagnitude;
           this.userAmount = userAmount;
           this.userMaxLimit = userMaxLimit;
    }


    /**
     * Gets the executionTime value for this ResponseParameter.
     * 
     * @return executionTime
     */
    public final java.lang.String getExecutionTime() {
        return executionTime;
    }


    /**
     * Sets the executionTime value for this ResponseParameter.
     * 
     * @param executionTime
     */
    public final void setExecutionTime(final java.lang.String executionTime) {
        this.executionTime = executionTime;
    }


    /**
     * Gets the result value for this ResponseParameter.
     * 
     * @return result
     */
    public final DataMatrix getResult() {
        return result;
    }


    /**
     * Sets the result value for this ResponseParameter.
     * 
     * @param result
     */
    public final void setResult(final DataMatrix result) {
        this.result = result;
    }


    /**
     * Gets the serviceMagnitude value for this ResponseParameter.
     * 
     * @return serviceMagnitude
     */
    public final int getServiceMagnitude() {
        return serviceMagnitude;
    }


    /**
     * Sets the serviceMagnitude value for this ResponseParameter.
     * 
     * @param serviceMagnitude
     */
    public final void setServiceMagnitude(final int serviceMagnitude) {
        this.serviceMagnitude = serviceMagnitude;
    }


    /**
     * Gets the userAmount value for this ResponseParameter.
     * 
     * @return userAmount
     */
    public final int getUserAmount() {
        return userAmount;
    }


    /**
     * Sets the userAmount value for this ResponseParameter.
     * 
     * @param userAmount
     */
    public final void setUserAmount(final int userAmount) {
        this.userAmount = userAmount;
    }


    /**
     * Gets the userMaxLimit value for this ResponseParameter.
     * 
     * @return userMaxLimit
     */
    public final int getUserMaxLimit() {
        return userMaxLimit;
    }


    /**
     * Sets the userMaxLimit value for this ResponseParameter.
     * 
     * @param userMaxLimit
     */
    public final void setUserMaxLimit(final int userMaxLimit) {
        this.userMaxLimit = userMaxLimit;
    }

    private java.lang.Object equalsCalc = null;
    public final synchronized boolean equals(final java.lang.Object obj) {
        if (!(obj instanceof ResponseParameter)) { return false; }
        ResponseParameter other = (ResponseParameter) obj;
        /* dead code: if (obj == null) { return false; } */
        if (this == obj) { return true; }
        if (equalsCalc != null) {
            return (equalsCalc == obj);
        }
        equalsCalc = obj;
        boolean equals;
        equals =  
            (
            		(this.executionTime == null && other.getExecutionTime() == null) 
            		||
            		(this.executionTime != null && this.executionTime.equals(other.getExecutionTime()))
             ) && (
            		 (this.result == null && other.getResult() == null) 
            		 || 
            		 (this.result != null && this.result.equals(other.getResult()))
            )
            && this.serviceMagnitude == other.getServiceMagnitude()
            && this.userAmount == other.getUserAmount()
            && this.userMaxLimit == other.getUserMaxLimit();
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
        if (getExecutionTime() != null) {
            hashCode += getExecutionTime().hashCode();
        }
        if (getResult() != null) {
            hashCode += getResult().hashCode();
        }
        hashCode += getServiceMagnitude();
        hashCode += getUserAmount();
        hashCode += getUserMaxLimit();
        hashCodeCalc = false;
        return hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ResponseParameter.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("urn:Synonyms", "ResponseParameter"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("executionTime");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "executionTime"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setNillable(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("result");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "result"));
        elemField.setXmlType(new javax.xml.namespace.QName("urn:Synonyms", "DataMatrix"));
        elemField.setNillable(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("serviceMagnitude");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "serviceMagnitude"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"));
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("userAmount");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "userAmount"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"));
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("userMaxLimit");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "userMaxLimit"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"));
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
