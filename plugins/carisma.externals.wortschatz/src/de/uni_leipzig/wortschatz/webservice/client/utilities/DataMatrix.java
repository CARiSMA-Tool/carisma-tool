/**
 * DataMatrix.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.utilities;

public class DataMatrix  implements java.io.Serializable {
    private DataVector[] dataVectors;

    public DataMatrix() {
    }

    public DataMatrix(
    		final DataVector[] dataVectors) {
           this.dataVectors = dataVectors;
    }


    /**
     * Gets the dataVectors value for this DataMatrix.
     * 
     * @return dataVectors
     */
    public final DataVector[] getDataVectors() {
        return dataVectors;
    }


    /**
     * Sets the dataVectors value for this DataMatrix.
     * 
     * @param dataVectors
     */
    public final void setDataVectors(final DataVector[] dataVectors) {
        this.dataVectors = dataVectors;
    }

    public final DataVector getDataVectors(final int i) {
        return this.dataVectors[i];
    }

    public final void setDataVectors(final int i, final DataVector value) {
        this.dataVectors[i] = value;
    }

    private java.lang.Object equalsCalc = null;
    public final synchronized boolean equals(final java.lang.Object obj) {
        if (!(obj instanceof DataMatrix)) { return false; }
        DataMatrix other = (DataMatrix) obj;
        /* dead code: if (obj == null) { return false; } */
        if (this == obj) { return true; }
        if (equalsCalc != null) {
            return (equalsCalc == obj);
        }
		equalsCalc = obj;
		boolean equals;
		equals = (
				(this.dataVectors == null && other.getDataVectors() == null) 
				|| (this.dataVectors != null && java.util.Arrays.equals(this.dataVectors, other.getDataVectors()))
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
        if (getDataVectors() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getDataVectors());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getDataVectors(), i);
				if (obj != null && !obj.getClass().isArray()) {
					hashCode += obj.hashCode();
				}
            }
        }
        hashCodeCalc = false;
        return hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(DataMatrix.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("urn:Synonyms", "DataMatrix"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dataVectors");
        elemField.setXmlName(new javax.xml.namespace.QName("urn:Synonyms", "dataVectors"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://datatypes.webservice.wortschatz.uni_leipzig.de", "DataVector"));
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
