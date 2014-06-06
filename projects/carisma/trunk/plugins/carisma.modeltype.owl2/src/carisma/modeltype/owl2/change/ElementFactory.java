/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.modeltype.owl2.change;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;

import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.AnnotationProperty;
import carisma.modeltype.owl2.model.owl.Class;
import carisma.modeltype.owl2.model.owl.ClassAssertion;
import carisma.modeltype.owl2.model.owl.Constant;
import carisma.modeltype.owl2.model.owl.DataProperty;
import carisma.modeltype.owl2.model.owl.DataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.Datatype;
import carisma.modeltype.owl2.model.owl.Declaration;
import carisma.modeltype.owl2.model.owl.NamedIndividual;
import carisma.modeltype.owl2.model.owl.ObjectProperty;
import carisma.modeltype.owl2.model.owl.ObjectPropertyAssertion;
import carisma.modeltype.owl2.model.owl.OwlFactory;
import carisma.modeltype.owl2.model.owl.URI;
import carisma.modeltype.owl2.type.OWL2XMLPrimitiveType;

public class ElementFactory {

	public static EObject createElement(ElementType type, String propertyString) {
		switch (type) {
		case ANNOTATION: 
			return createAnnotationByConstant(propertyString); 
		case CLASS_ASSERTION:
			return createClassAssertion(propertyString);
		case DATA_PROPERTY_ASSERTION:
			return createDataPropertyAssertion(propertyString);
		case INDIVIDUAL:
			return createNamedIndividual(propertyString);
		case INDIVIDUAL_DECLARATION:
			return createIndividualDeclaration(propertyString);
		case OBJECT_PROPERTY_ASSERTION:
			return createObjectPropertyAssertion(propertyString);
		default:
			System.err.println("Error: No Template for '" + type.getValue() + "' defined.");
			break;
		}
		return null;
	}
	
	private static DataPropertyAssertion createDataPropertyAssertion(String propertyString) {
		String[] propArr = propertyString.split(",");
		if (propArr.length == 3) {
			OwlFactory factory = OwlFactory.eINSTANCE;
			
			DataPropertyAssertion dataPropertyAssertion = factory.createDataPropertyAssertion();
			dataPropertyAssertion.setAxiomId(EcoreUtil.generateUUID());
			
			URI dataTypeUri = factory.createURI();
			URI dataPropertyUri = factory.createURI();
			dataTypeUri.setValue(OWL2XMLPrimitiveType.STRING.getValue());
			dataPropertyUri.setValue(propArr[0]);
			
			Datatype datatype = factory.createDatatype();
			datatype.setArity(1);
			datatype.setEntityURI(dataTypeUri);
			
			Constant constant = factory.createConstant();
			constant.setLexicalValue(propArr[2]);
			constant.setDatatype(datatype);
			
			DataProperty dataProperty = factory.createDataProperty();
			dataProperty.setEntityURI(dataPropertyUri);
			
			dataPropertyAssertion.setDataPropertyExpression(dataProperty);
			dataPropertyAssertion.setSourceIndividual(createNamedIndividual(propArr[1]));
			dataPropertyAssertion.setTargetValue(constant);
			
			return dataPropertyAssertion;
		} else {
			System.err.println("Error: Parsing error in annotation string.");
		return null;
		}
	}

	private static ClassAssertion createClassAssertion(String propertyString) {
		String[] uriArr = propertyString.split(",");
		if (uriArr.length == 2) {
			OwlFactory factory = OwlFactory.eINSTANCE;
			
			ClassAssertion classAssertion = factory.createClassAssertion();
			classAssertion.setAxiomId(EcoreUtil.generateUUID());
			
			URI uri = factory.createURI();
			uri.setValue(uriArr[0].trim());
			
			Class owlClass = factory.createClass();
			owlClass.setEntityURI(uri);
			
			classAssertion.setClassExpression(owlClass);
			classAssertion.setIndividual(createNamedIndividual(uriArr[1]));
			
			return classAssertion;
		} else {
			System.err.println("Error: Parsing error in annotation string.");
		return null;
		}
	}

	private static ObjectPropertyAssertion createObjectPropertyAssertion(String propertyString) {
		String[] uriArr = propertyString.split(",");
		if (uriArr.length == 3) {
			OwlFactory factory = OwlFactory.eINSTANCE;
			
			ObjectPropertyAssertion objPropAssertion = factory.createObjectPropertyAssertion();
			objPropAssertion.setAxiomId(EcoreUtil.generateUUID());

			URI uri = factory.createURI();
			uri.setValue(uriArr[0].trim());
			
			ObjectProperty objProp = factory.createObjectProperty();
			objProp.setEntityURI(uri);
			
			objPropAssertion.setObjectPropertyExpression(objProp);
			objPropAssertion.setSourceIndividual(createNamedIndividual(uriArr[1]));
			objPropAssertion.setTargetIndividual(createNamedIndividual(uriArr[2]));
			
			return objPropAssertion;
		} else {
			System.err.println("Error: Parsing error in annotation string.");
			return null;
		}
	}

	private static Declaration createIndividualDeclaration(String propertyString) {
		OwlFactory factory = OwlFactory.eINSTANCE;
		Declaration declaration = factory.createDeclaration();
		declaration.setEntity(createNamedIndividual(propertyString));
		declaration.setAxiomId(EcoreUtil.generateUUID());
		return declaration;
	}

	private static NamedIndividual createNamedIndividual(String propertyString) {
		OwlFactory factory = OwlFactory.eINSTANCE;
		URI uri = factory.createURI();
		uri.setValue(propertyString.trim());
		NamedIndividual individual = factory.createNamedIndividual();
		individual.setEntityURI(uri);
		return individual;
	}

	private static AnnotationByConstant createAnnotationByConstant(String propertyString) {
		OwlFactory factory = OwlFactory.eINSTANCE;
		String[] values = propertyString.split(",");
		if (values.length == 2) {
			AnnotationByConstant annotation = factory.createAnnotationByConstant();
			
			AnnotationProperty property = factory.createAnnotationProperty();
			URI propertyURI = factory.createURI();
			
			Constant constant = factory.createConstant();

			Datatype datatype = factory.createDatatype();
			URI dataTypeURI = factory.createURI();

			dataTypeURI.setValue(OWL2XMLPrimitiveType.STRING.getValue());
			datatype.setArity(1);
			datatype.setEntityURI(dataTypeURI);

			propertyURI.setValue(values[1].trim());
			property.setEntityURI(propertyURI);
			
			constant.setLexicalValue(values[0].trim());
			constant.setDatatype(datatype);
			
			annotation.setAnnotationProperty(property);
			annotation.setAnnotationValue(constant);
			
			return annotation;
		} else {
			System.err.println("Error: Parsing error in annotation string.");
			return null;
		}
	}
	
}
