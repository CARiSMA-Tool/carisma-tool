package carisma.xutils.regulatory.ui.controller;

import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

import carisma.xutils.regulatory.ui.model.Data;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;


// TODO: Auto-generated Javadoc
/**
 * Controller for communication between GUI and Data class.
 *
 * @author bm
 */
public class DataController {

    /**
     * creates a new Data-controller.
     */
    public DataController() {

    }

    /** The data. */
    private Data data = Data.instance();

    /**
     * set color at given index of the colorList.
     *
     * @param color the color
     * @param ruleIndex the rule index
     */
    public final void setColor(final Color color, final RULEELEMENTS ruleIndex) {
        data.setColor(color.getRGB(), ruleIndex);
    }

    /**
     * get color at given index of the colorList.
     *
     * @param ruleIndex the rule index
     * @return RGB
     */
    public final RGB getColor(final RULEELEMENTS ruleIndex) {
        return data.getColor(ruleIndex);
    }

    /**
     * deletes the items in the lists of RuleElements.
     */
    public final void clearRuleElements() {
        data.clearRole();
        data.clearActivity();
        data.clearProperty();
        data.clearProcess();
        data.clearArtifact();
    }

    /**
     * adds item to correspondent list of RuleElements.
     *
     * @param ruleElementText the rule element text
     * @param ruleElementIndex the rule element index
     */
    public final void addRuleElement(final String ruleElementText, final int ruleElementIndex) {
        if (ruleElementIndex == 0) {
			data.addRole(ruleElementText);
		}
        if (ruleElementIndex == 1) {
			data.addActivity(ruleElementText);
		}
        if (ruleElementIndex == 2) {
			data.addProperty(ruleElementText);
		}
        if (ruleElementIndex == 3) {
			data.addProcess(ruleElementText);
		}
        if (ruleElementIndex == 4) {
			data.addArtifact(ruleElementText);
		}
    }

	public List<String> getRole() {
		return data.getRole();
	}

	public List<String> getActivity() {
		return data.getActivity();
	}

	public List<String> getProperty() {
		return data.getProperty();
	}

	public List<String> getProcess() {
		return data.getProcess();
	}

	public List<String> getArtifact() {
		return data.getArtifact();
	}

	public String[] listToStringArray(List<String> list) {
		return data.listToStringArray(list);
	}
}
