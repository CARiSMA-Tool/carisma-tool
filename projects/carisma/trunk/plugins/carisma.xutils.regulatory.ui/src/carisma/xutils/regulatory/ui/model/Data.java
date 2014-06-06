package carisma.xutils.regulatory.ui.model;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import carisma.regulatory.ontology.Role;

// TODO: Auto-generated Javadoc
/**
 * Helper class to store lists of RuleElements and colors.
 *
 * @author bm
 */
// TODO vernuenftiges JavaDoc
public class Data {

    /** The instance. */
    private static Data instance;
    
    /** The color map. */
    private Map<String, RGB> colorMap = null;

    // lists of RuleElements
    /** The role. */
    private List<String> role;
    
    /** The activity. */
    private List<String> activity;
    
    /** The property. */
    private List<String> property;
    
    /** The process. */
    private List<String> process;
    
    /** The artifact. */
    private List<String> artifact;

    /**
     * creates new lists. private constructor - you cannot create new objects
     * from outside of this class
     */
    private Data() {
    	colorMap = new HashMap<String, RGB>();
    	colorMap.put(RULEELEMENTS.Role.name(), new RGB(255, 0, 0));
    	colorMap.put(RULEELEMENTS.Activity.name(), new RGB(0, 0, 255));
    	colorMap.put(RULEELEMENTS.Property.name(), new RGB(238, 238, 0));
    	colorMap.put(RULEELEMENTS.Process.name(), new RGB(0, 180, 0));
    	colorMap.put(RULEELEMENTS.Artifact.name(), new RGB(255, 165, 0));
    	
        role = new LinkedList<String>();
        activity = new LinkedList<String>();
        property = new LinkedList<String>();
        process = new LinkedList<String>();
        artifact = new LinkedList<String>();
    }

    /**
     * returns the only instance of this class.
     *
     * @return Data
     */
    public static Data instance() {
        if (instance == null) {
            instance = new Data();
        }
        return instance;
    }

    /**
     * sets the new color of a given rule element.
     *
     * @param color the color
     * @param ruleElement the rule element
     */
    public void setColor(final RGB color, final RULEELEMENTS ruleElement) {
    	colorMap.put(ruleElement.name(), color);
    }

    /**
     * returns the color of a given rule element.
     *
     * @param ruleElement the rule element
     * @return {@link RGB}
     */
    public RGB getColor(final RULEELEMENTS ruleElement) {
        return colorMap.get(ruleElement.name());
    }

//    public RGB[] getColorList() {		// not used
//        return colorList;
//   }

    /**
     * adds a role to the list.
     *
     * @param s the s
     */
    public void addRole(final String s) {
        role.add(s);
    }

    /**
     * returns the list of roles.
     * @return {@link List} of {@link Role}
     */
    public List<String> getRole() {
        return Collections.unmodifiableList(role);
    }

    /**
     * clears the role list.
     */
    public void clearRole() {
        role.clear();
    }

    /**
     * Adds the activity.
     *
     * @param s the s
     */
    public void addActivity(final String s) {
        activity.add(s);
    }

    /**
     * Gets the activity.
     *
     * @return the activity
     */
    public List<String> getActivity() {
        return Collections.unmodifiableList(activity);
    }

    /**
     * Clear activity.
     */
    public void clearActivity() {
        activity.clear();
    }

    /**
     * Adds the property.
     *
     * @param s the s
     */
    public void addProperty(final String s) {
        property.add(s);
    }

    /**
     * Gets the property.
     *
     * @return the property
     */
    public List<String> getProperty() {
        return Collections.unmodifiableList(property);
    }

    /**
     * Clear property.
     */
    public void clearProperty() {
        property.clear();
    }

    /**
     * Adds the process.
     *
     * @param s the s
     */
    public void addProcess(final String s) {
        process.add(s);
    }

    /**
     * Gets the process.
     *
     * @return the process
     */
    public List<String> getProcess() {
        return Collections.unmodifiableList(process);
    }

    /**
     * Clear process.
     */
    public void clearProcess() {
        process.clear();
    }

    /**
     * Adds the artifact.
     *
     * @param s the s
     */
    public void addArtifact(final String s) {
        artifact.add(s);
    }

    /**
     * Gets the artifact.
     *
     * @return the artifact
     */
    public List<String> getArtifact() {
        return Collections.unmodifiableList(artifact);
    }

    /**
     * Clear artifact.
     */
    public void clearArtifact() {
        artifact.clear();
    }

    /**
     * converts List to StringArray.
     *
     * @param l the l
     * @return String[]
     */
    public String[] listToStringArray(final List<String> l) {
        String[] array = l.toArray(new String[l.size()]);
        return array;
    }

}
