package carisma.core.preferences;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class PreferenceManager {

	private final HashMap<String, PreferenceValue> values;
	private final HashMap<String, PreferenceValue> defaults;

	public PreferenceManager() {
		this.values = new HashMap<>();
		this.defaults = new HashMap<>();
	}

	public Object getPreferenceValue(final String name) {
		Object result = null;
		final PreferenceValue val = this.values.get(name);
		if (val != null) {
			result = val.getValue();
		} else {
			result = getDefaultPreferenceValue(name);
		}
		return result;
	}

	public void setPreferenceValue(final String name, final Object value) {
		final PreferenceValue val = this.values.get(name);
		if (val != null) {
			val.setValue(value);
		} else {
			this.values.put(name, new PreferenceValue(name, value));
		}
	}

	public Object getDefaultPreferenceValue(final String name) {
		Object result = null;
		final PreferenceValue val = this.defaults.get(name);
		if (val != null) {
			result = val.getValue();
		}
		return result;
	}

	public void setDefaultPreferenceValue(final String name, final Object value) {
		final PreferenceValue val = this.defaults.get(name);
		if (val != null) {
			val.setValue(value);
		} else {
			this.defaults.put(name, new PreferenceValue(name, value));
		}
	}

	public Object setDefaultAndGetValue(final String name, final Object value) {
		setDefaultAndGetValue(name, value);
		return getPreferenceValue(name);
	}

	private static XStream createXStream() {
		final XStream xStream = new XStream(new DomDriver());
		return xStream;
	}

	/**
	 * Writes an analysis configuration into a file.
	 * @param analysis
	 * @param filename
	 */
	public void storePreferences(final String filename, final boolean includeDefaults) {
		final XStream xStream = createXStream();
		try (FileOutputStream fos = new FileOutputStream(filename)){
			final ArrayList<PreferenceValue> vals = new ArrayList<>(this.values.values());
			if (includeDefaults) {
				for (final PreferenceValue pv : this.defaults.values()) {
					if (!this.values.containsKey(pv.getName())) {
						vals.add(pv);
					}
				}
			}
			xStream.toXML(vals.toArray(), fos);
		} catch (final FileNotFoundException e) {
			logFileNotFoundException(filename, e);
		} catch (final IOException e) {
			Logger.log(LogLevel.ERROR, "Error on closing \"" + filename, e);
		}
	}

	private static void logFileNotFoundException(final String filename, final FileNotFoundException e) {
		final File output = new File(filename);
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
	public void loadPreferences(final String filename) {
		final XStream xStream = createXStream();
		try (FileInputStream fis = new FileInputStream(filename);
				InputStreamReader isr = new InputStreamReader(fis, "ISO-8859-1");){
			this.values.clear();
			final Object[] loadedValues = (Object[])xStream.fromXML(isr);
			for (final Object val : loadedValues) {
				final PreferenceValue value = (PreferenceValue)val;
				this.values.put(value.getName(), value);
			}
		} catch (final FileNotFoundException e) {
			logFileNotFoundException(filename, e);
		} catch (final Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
		}
	}
}
