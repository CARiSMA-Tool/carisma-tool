package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import carisma.evolution.AddElement;

/** Wrapper class for AddElements used for Delta export.
 * 
 * @author bberghoff
 *
 */
public class ExportAddElement extends ExportAdditiveElement {

        /**
         * The typename of the element to be added.
         */
		private String typename;

		/**
		 * The property values of the new element.
		 */
		private Map<String, Object> values;
		/**
		 * The further content of the new element.
		 */
		private List<ExportAddElement> content;
		
		/**
		 * AddElement.toString().
		 */
		private String string = null;


		/** Public Constructor.
		 * @param addE .
		 */
		public ExportAddElement(final AddElement addE) {
			this.typename = addE.getMetaClass().getName();
            this.values = ExporterUtility.getValuesWithStringNull(addE.getValues());
			this.content = new ArrayList<>();
			for (AddElement ele : addE.getContent()) {
				this.content.add(new ExportAddElement(ele));
			}
			this.string = addE.toString();
		}

		/** Getter for the field 'typename'.
		 * @return the name of the type.
		 */
		public final String getType() {
			return this.typename;
		}

		/** Getter for the field 'content'.
		 * 
		 * @return the content of the AddElement.
		 */
		public final List<ExportAddElement> getContent() {
			return this.content;
		}
		
		/** Gett for the field 'values'.
		 * 
		 * @return the values of the AddElement.
		 */
		public final  Map<String, Object> getValues() {
			return this.values;
		}

		@Override
        public final String toString() {
			
			if (this.string != null) {
				return this.string;
			}
			StringBuffer output = new StringBuffer("- INSERT: ");
			output.append(this.getType());
			if (this.getValues().size() > 0) {
				output.append(" with ");
			}
			for (String key : this.getValues().keySet()) {
				output.append(", attribute ");
				output.append(key);
				output.append(" = '");
				output.append(this.getValues().get(key));
				output.append("'");
			}
			if (this.getValues().size() > 0) {
				output.append(". TO: ");
				output.append(this.getTarget().getType());
				output.append(" '");
				output.append(this.getTarget().getName());
				output.append("'");
			}
			return output.toString();
		}
}
