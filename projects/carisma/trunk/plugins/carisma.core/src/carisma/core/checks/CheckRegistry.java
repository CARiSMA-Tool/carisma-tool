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
package carisma.core.checks;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import carisma.core.Carisma;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


/**
 * Manages the analysis checks that are available. 
 * @author wenzel
 *
 */
public class CheckRegistry {

	private List<CheckDescriptor> registeredChecks;
	/**
	 * Constant value for the buffer size to start with and for increasing the actual buffer size.
	 */
	private static final int INITIAL_BUFFER_SIZE = 8192;
	
	public CheckRegistry() {
		this.registeredChecks = new ArrayList<>();
	}
	
	/**
	 * Queries the available checks from the Eclipse extension point registry.
	 * This method is called when the Carisma core is started. Users should not call this operation.
	 */
	public void initialize() {
		// search for adapters in ExtensionRegistry
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(Carisma.EXTENSION_POINT_CARISMA_CARISMACHECK);
		
		for (IExtension extension : extensionPoint.getExtensions()) { 
			for (IConfigurationElement checkElement : extension.getConfigurationElements()) {
				String id = checkElement.getAttribute("id");
				String name = checkElement.getAttribute("name");
				String publisher = checkElement.getAttribute("publisher");
				String targetModelType = checkElement.getAttribute("targetModelType");
				if (targetModelType == null || targetModelType.trim().equals("")) {
					targetModelType = "*";
				}
				String implementingClass = checkElement.getAttribute("implementingClass");
				String description = checkElement.getAttribute("description");
				String magicKeys = checkElement.getAttribute("magicKeys");				
				CheckDescriptor checkDescriptor = new CheckDescriptor(id, name, publisher, targetModelType, implementingClass, description, magicKeys);
				for (IConfigurationElement parameterElement : checkElement.getChildren("parameter")) {
					String pID = parameterElement.getAttribute("id"); 
					String pName = parameterElement.getAttribute("name"); 
					String pDescription = parameterElement.getAttribute("description"); 
					boolean pOptional = Boolean.parseBoolean(parameterElement.getAttribute("optional")); 
					ParameterType pType = ParameterType.byName(parameterElement.getAttribute("type"));
					String defaultValue = parameterElement.getAttribute("defaultValue");
					if ("".equals(defaultValue)) {
						defaultValue = null;
					}
					CheckParameterDescriptor parameterDescriptor = new CheckParameterDescriptor(pID, pName, pDescription, pType, pOptional, defaultValue);
					checkDescriptor.getParameters().add(parameterDescriptor);			
				}
				for (IConfigurationElement parameterElement : checkElement.getChildren("precondition")) {
					String key = parameterElement.getAttribute("requiredKey");
					checkDescriptor.getRequiredKeys().add(key);
				}
				for (IConfigurationElement parameterElement : checkElement.getChildren("postcondition")) {
					String key = parameterElement.getAttribute("providedKey");
					checkDescriptor.getProvidedKeys().add(key);
				}
				this.registeredChecks.add(checkDescriptor);
				Logger.log(LogLevel.DEBUG, "Check '" + id + "' has been registered.");
				String[] targetModelTypes = targetModelType.split(",");
				for (String modelType : targetModelTypes) {
					if (!modelType.equals("*") && !Carisma.getInstance().getModelTypeRegistry().isSupportedType(modelType)) {
						Logger.log(LogLevel.WARNING, "Check '" + id + "' is targeted for unsupported model type '" + modelType + "'.");
					}
				}
			}
		}

	}
	
	/**
	 * Returns a list of check descriptors describing the available check.
	 * @return
	 */
	public List<CheckDescriptor> getRegisteredChecks() {
		return Collections.unmodifiableList(this.registeredChecks);
	}
	
	/**
	 * Returns a list of check descriptors of all checks applicable to models of the given model type.
	 * @param modelType
	 * @return
	 */
	public List<CheckDescriptor> findChecks(String modelType) {
		ArrayList<CheckDescriptor> result = new ArrayList<>();
		for (CheckDescriptor cd : this.registeredChecks) {
			String[] modelTypes = cd.getTargetModelType().split(",");
			for (String mt : modelTypes) {
				if (modelType.equalsIgnoreCase(mt)
						&& (!result.contains(cd))) {
					result.add(cd);
				}
			}
		}
		return result;
	}
	
	/**
	 * Returns the check descriptor for the check with the given id.
	 * @param id
	 * @return
	 */
	public CheckDescriptor getCheckDescriptor(String id) {
		for (CheckDescriptor cd : this.registeredChecks) {
			if (cd.getCheckDescriptorId().equalsIgnoreCase(id)) {
				return cd;
			}
		}
		return null;
	}
	
	/**
	 * Creates and returns an instance of the specified check. 
	 * @param checkDescriptor
	 * @return
	 */
	public static CarismaCheckWithID getCheck(CheckDescriptor checkDescriptor) {
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(Carisma.EXTENSION_POINT_CARISMACHECK);
		for (IExtension extension : extensionPoint.getExtensions()) {
			for (IConfigurationElement checkElement : extension.getConfigurationElements()) {
				if (checkDescriptor.getCheckDescriptorId().equalsIgnoreCase(checkElement.getAttribute("id"))) {
					try {
						return (CarismaCheckWithID) checkElement.createExecutableExtension("implementingClass");
					} catch (CoreException e) {
						Logger.log(LogLevel.ERROR, "Unable to load check class: " + checkDescriptor.getCheckDescriptorId(), e);
					}
				}
			}
		}
		return null;
	}
	
	/**
	 * Creates a check reference object for a given check descriptor. The reference can be used in
	 * analysis definitions and refers to the check. 
	 * @param descriptor
	 * @return
	 */
	public static  CheckReference createReference(CheckDescriptor descriptor) {
		CheckReference reference = new CheckReference(descriptor.getCheckDescriptorId(), true);
		for (CheckParameterDescriptor cpd : descriptor.getParameters()) {
			if (cpd.getType() == ParameterType.STRING) {
				StringParameter param = new StringParameter(cpd);
				if (cpd.getDefaultValue() != null) {
					param.setValue(cpd.getDefaultValue());
				}
				reference.getParameters().add(param);
			} else if (cpd.getType() == ParameterType.BOOLEAN) {
				BooleanParameter param = new BooleanParameter(cpd);
				if (cpd.getDefaultValue() != null) {
					param.setValue(Boolean.parseBoolean(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			} else if (cpd.getType() == ParameterType.INTEGER) {
				IntegerParameter param = new IntegerParameter(cpd);
				if (cpd.getDefaultValue() != null) {
//FIXME: What if the default value is empty?
					param.setValue(Integer.parseInt(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			} else if (cpd.getType() == ParameterType.FLOAT) {
				FloatParameter param = new FloatParameter(cpd);
				if (cpd.getDefaultValue() != null) {
//FIXME: What if the default value is empty?
					param.setValue(Float.parseFloat(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			}
			if (cpd.getType() == ParameterType.INPUTFILE) {
				InputFileParameter param = new InputFileParameter(cpd);
				if (cpd.getDefaultValue() != null) {
					param.setValue(new File(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			}
			if (cpd.getType() == ParameterType.OUTPUTFILE) {
				OutputFileParameter param = new OutputFileParameter(cpd); // ?
				if (cpd.getDefaultValue() != null) {
					param.setValue(new File(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			}
			if (cpd.getType() == ParameterType.FOLDER) {
				FolderParameter param = new FolderParameter(cpd);  // ??
				if (cpd.getDefaultValue() != null) {
					param.setValue(new File(cpd.getDefaultValue()));
				}
				reference.getParameters().add(param);
			}
		}
		return reference;
	}
	
	/**
	 * Method reads the model file. Search for all magicKeys if they can be found
	 * in the model file
	 * @param modelIFile
	 * @return list of recommended Checks for current model file
	 */
	public List<CheckDescriptor> getRecommendedChecks(IFile modelIFile) {
		String content = "";
		try {
			if (modelIFile.exists()) {
				
				// Ress might be out of sync if the file is modified outside of eclipse
				modelIFile.refreshLocal(IResource.DEPTH_ZERO, null);
				
				try(InputStream is = modelIFile.getContents()){
					int buffersize = INITIAL_BUFFER_SIZE; // 8K, default size
					byte[] buf = new byte[buffersize];				
					int bytesred = 0;
					while (is.available() != 0) { 
						bytesred += is.read(buf, bytesred, INITIAL_BUFFER_SIZE);
						if (bytesred == buffersize)  {
							buffersize += INITIAL_BUFFER_SIZE;
							byte[] newBuf = new byte[buffersize];
							System.arraycopy(buf, 0, newBuf, 0, bytesred);
							buf = newBuf;
						}													
					}
					content = new String(buf, 0, bytesred);
				} catch (IOException e) {
					Logger.log(LogLevel.ERROR, "Cannot read file", e);
					
				}
			}
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "Cannot find or read file" ,	e);
		}
		List<CheckDescriptor> recChecks = new ArrayList<>();
		
		List<CheckDescriptor> regchecks = getRegisteredChecks();
		for (CheckDescriptor check : regchecks) {
			String mkeys = check.getMagicKeys();
			if (mkeys != null) {
				String[] key = mkeys.split(",");
				for (int i = 0; i < key.length; i++) {
					key[i] = key[i].trim();

					if (content.contains(key[i])) {   
						recChecks.add(check);
						break;
					}
				}
			}			
		}
		return recChecks;
	}
}
