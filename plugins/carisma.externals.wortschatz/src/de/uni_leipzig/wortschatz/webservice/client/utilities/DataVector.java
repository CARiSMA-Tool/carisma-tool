/**
 * DataVector.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2RC3 Mar 28, 2005 (10:34:47 CEST) WSDL2Java emitter.
 */

package de.uni_leipzig.wortschatz.webservice.client.utilities;

public class DataVector implements java.io.Serializable {
	private java.lang.String[] dataRow;

	public DataVector() {
	}

	public DataVector(final java.lang.String[] dataRow) {
		this.dataRow = dataRow;
	}

	/**
	 * Gets the dataRow value for this DataVector.
	 * 
	 * @return dataRow
	 */
	public final java.lang.String[] getDataRow() {
		return dataRow;
	}

	/**
	 * Sets the dataRow value for this DataVector.
	 * 
	 * @param dataRow
	 */
	public final void setDataRow(final java.lang.String[] dataRow) {
		this.dataRow = dataRow;
	}

	public final java.lang.String getDataRow(final int i) {
		return this.dataRow[i];
	}

	public final void setDataRow(final int i, final java.lang.String value) {
		this.dataRow[i] = value;
	}

	private java.lang.Object equalsCalc = null;

	@Override
	public final synchronized boolean equals(final java.lang.Object obj) {
		if (!(obj instanceof DataVector)) {
			return false;
		}
		DataVector other = (DataVector) obj;
		/* dead code: if (obj == null) { return false; } */
		if (this == obj) {
			return true;
		}
		if (equalsCalc != null) {
			return (equalsCalc == obj);
		}
		equalsCalc = obj;
		boolean equals;
		equals = ((this.dataRow == null && other.getDataRow() == null) || (this.dataRow != null && java.util.Arrays.equals(this.dataRow, other.getDataRow())));
		equalsCalc = null;
		return equals;
	}

	private boolean hashCodeCalc = false;

	@Override
	public final synchronized int hashCode() {
		if (hashCodeCalc) {
			return 0;
		}
		hashCodeCalc = true;
		int hashCode = 1;
		if (getDataRow() != null) {
			for (int i = 0; i < java.lang.reflect.Array.getLength(getDataRow()); i++) {
				java.lang.Object obj = java.lang.reflect.Array.get(getDataRow(), i);
				if (obj != null && !obj.getClass().isArray()) {
					hashCode += obj.hashCode();
				}
			}
		}
		hashCodeCalc = false;
		return hashCode;
	}

	// Type metadata
	private static org.apache.axis.description.TypeDesc typeDesc = new org.apache.axis.description.TypeDesc(DataVector.class, true);

	static {
		typeDesc.setXmlType(new javax.xml.namespace.QName("http://datatypes.webservice.wortschatz.uni_leipzig.de", "DataVector"));
		org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
		elemField.setFieldName("dataRow");
		elemField.setXmlName(new javax.xml.namespace.QName("http://datatypes.webservice.wortschatz.uni_leipzig.de", "dataRow"));
		elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
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
	public static org.apache.axis.encoding.Serializer getSerializer(final java.lang.String mechType, final java.lang.Class javaType, final javax.xml.namespace.QName xmlType) {
		return new org.apache.axis.encoding.ser.BeanSerializer(javaType, xmlType, typeDesc);
	}

	/**
	 * Get Custom Deserializer.
	 */
	public static org.apache.axis.encoding.Deserializer getDeserializer(final java.lang.String mechType, final java.lang.Class javaType, final javax.xml.namespace.QName xmlType) {
		return new org.apache.axis.encoding.ser.BeanDeserializer(javaType, xmlType, typeDesc);
	}

}
