package carisma.core.preferences;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class PreferenceManager {

	private HashMap<String, PreferenceValue> values;
	private HashMap<String, PreferenceValue> defaults;
	
	public PreferenceManager() {
		this.values = new HashMap<>();
		this.defaults = new HashMap<>();
	}
	
	public Object getPreferenceValue(String name) {
		Object result = null;
		PreferenceValue val = this.values.get(name);
		if (val != null) {
			result = val.getValue();
		} else {
			result = getDefaultPreferenceValue(name);
		}
		return result;
	}
	
	public void setPreferenceValue(String name, Object value) {
		PreferenceValue val = this.values.get(name);
		if (val != null) {
			val.setValue(value);
		} else {
			this.values.put(name, new PreferenceValue(name, value));
		}
	}
	
	public Object getDefaultPreferenceValue(String name) {
		Object result = null;
		PreferenceValue val = this.defaults.get(name);
		if (val != null) {
			result = val.getValue();
		}
		return result;
	}
	
	public void setDefaultPreferenceValue(String name, Object value) {
		PreferenceValue val = this.defaults.get(name);
		if (val != null) {
			val.setValue(value);
		} else {
			this.defaults.put(name, new PreferenceValue(name, value));
		}
	}
	
	public Object setDefaultAndGetValue(String name, Object value) {
		setDefaultAndGetValue(name, value);
		return getPreferenceValue(name);
	}
	
	private static XStream createXStream() {
		XStream xStream = new XStream(new DomDriver());
		return xStream;
	}

	/**
	 * Writes an analysis configuration into a file.
	 * @param analysis
	 * @param filename
	 */
	public void storePreferences(String filename, boolean includeDefaults) {
		XStream xStream = createXStream();
		try (FileOutputStream fos = new FileOutputStream(filename)){
			ArrayList<PreferenceValue> vals = new ArrayList<>();
			vals.addAll(this.values.values());
			if (includeDefaults) {
				for (PreferenceValue pv : this.defaults.values()) {
					if (!this.values.containsKey(pv.getName())) {
						vals.add(pv);
					}
				}
			}
			xStream.toXML(vals.toArray(), fos);
		} catch (FileNotFoundException e) {
			logFileNotFoundException(filename, e);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "Error on closing \"" + filename, e);
		}
	}
	
	private static void logFileNotFoundException(String filename, FileNotFoundException e) {
		File output = new File(filename);
		if (output.isDirectory()) {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" is a directory. Please specify a correct file", e);
		} else if (!output.exists()) {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" cannot be created for some reasons", e);
		} else {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" cannot be opend for some reasons", e);
		}
	}
	
	/**
	 * Reads an analysis configuration from a file.
	 * @param filename
	 * @return
	 */
	public void loadPreferences(String filename) {
		XStream xStream = createXStream();
		try (FileInputStream fis = new FileInputStream(filename);
				InputStreamReader isr = new InputStreamReader(fis, "ISO-8859-1");){
			this.values.clear();
			Object[] loadedValues = (Object[])xStream.fromXML(isr);
			for (Object val : loadedValues) {
				PreferenceValue value = (PreferenceValue)val;
				this.values.put(value.getName(), value);
			}
		} catch (FileNotFoundException e) {
			logFileNotFoundException(filename, e);
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
		}
	}
	
	public static void main(String[] args) {
		PreferenceManager pm = new PreferenceManager();
		pm.setPreferenceValue("WS_expansion_cache", "c:\\meier\\excache.ser");
		pm.setPreferenceValue("WS_normalizer_cache", "c:\\meier\\normcache.ser");
		pm.storePreferences("/tmp/pm.props", false);
		pm.setPreferenceValue("WS_expansion_cache", "lost");
		pm.loadPreferences("/tmp/pm.props");
		System.out.println(pm.getPreferenceValue("WS_expansion_cache"));
	}
}
