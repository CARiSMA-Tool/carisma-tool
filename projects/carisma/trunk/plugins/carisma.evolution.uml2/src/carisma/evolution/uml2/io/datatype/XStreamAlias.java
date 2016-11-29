package carisma.evolution.uml2.io.datatype;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import carisma.evolution.Delta;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.xml.DomDriver;


public final class XStreamAlias {

	/**
	 * Hiding constructor.
	 */
	private XStreamAlias() {
	}
	
	/** 
	 * The name of the field used by the .toString() Method.
	 */
	private static final String FIELD_STRING = "string";
	
	/** Returns an modified XStream Object with pre-set aliases and a MapConverter.
	 * 
	 * @return XStream object.
	 */
	public static XStream getXStream() {
		
		XStream stream = new XStream(new DomDriver());
		stream.setClassLoader(ExportAddElement.class.getClassLoader());
		stream.alias("AddElement", ExportAddElement.class);
		stream.alias("DelElement", ExportDelElement.class);
		stream.alias("EditElement", ExportEditElement.class);
		stream.alias("SubstElement", ExportSubstElement.class);
		stream.alias("CopyElement", ExportCopyElement.class);
		stream.alias("Delta", ExportDelta.class);
		stream.aliasField("ExtendedElement", ExportExtTagStereotype.class, "extendedElement");
		stream.aliasField("ExtendedElement", ExportExtTagTaggedValue.class, "extendedElement");

		stream.omitField(ExportAddElement.class, FIELD_STRING);
		stream.omitField(ExportDelElement.class, FIELD_STRING);
		stream.omitField(ExportEditElement.class, FIELD_STRING);
		stream.omitField(ExportSubstElement.class, FIELD_STRING);
		stream.omitField(ExportCopyElement.class, FIELD_STRING);
		
		stream.addImplicitCollection(Delta.class, "deltaContent");
		
		stream.alias("NamedElement", ExportExtTagNamedElement.class);
		stream.alias("StereotypeApplication", ExportExtTagStereotype.class);
		stream.alias("TaggedValue", ExportExtTagTaggedValue.class);
		stream.alias("Deltas", List.class);
		
	 	stream.registerConverter(new MapEntryConverter());
		return stream;
	}
}


class MapEntryConverter implements Converter{
	@Override
	@SuppressWarnings("rawtypes")
	public boolean canConvert(Class clazz) {
		return HashMap.class.isAssignableFrom(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public void marshal(Object value, HierarchicalStreamWriter writer, MarshallingContext context) {
	    AbstractMap<String, Object> map = (AbstractMap<String, Object>) value;
	    for (Entry<String, Object> entry : map.entrySet()) {
	    	writer.startNode("entry");
	        writer.startNode("key");
	        writer.setValue(entry.getKey().toString());
	        writer.endNode();
	        writer.startNode("value");
	        writer.setValue(entry.getValue().toString());
	        writer.endNode();
	        writer.endNode();
	    }
	}
	
	@Override
	public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
	    Map<String, Object> map = new HashMap<>();
	
	    while (reader.hasMoreChildren()) {
	        reader.moveDown();
	        reader.moveDown();
	        String key = reader.getValue();
	        String value  = null;

            reader.moveUp();
            reader.moveDown();
            value = reader.getValue();
            reader.moveUp();
	        map.put(key, value);
	        reader.moveUp();
	        
	    }
	    return map;
	}
}