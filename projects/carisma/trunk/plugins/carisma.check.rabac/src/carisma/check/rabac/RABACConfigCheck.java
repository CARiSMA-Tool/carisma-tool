package carisma.check.rabac;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JComboBox;
import javax.swing.JCheckBox;
import javax.swing.JTextField;
import javax.swing.JButton;
import javax.swing.DefaultComboBoxModel;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Transition;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheck;
import carisma.profile.umlsec.rabac.UMLsec;
import carisma.profile.umlsec.rabac.UMLsecUtil;

public class RABACConfigCheck implements CarismaCheck, ActionListener {
	AnalysisHost host;
	Map<String, CheckParameter> parameters;

	RABACConfig config = new RABACConfig();

	private List<String> usersTag;
	private Set<String> users, objects;

	private JComboBox<String> user, role, object, attribute;
	private JComboBox<String> type = new JComboBox<String>(new String[] { "User", "Object" });
	private JCheckBox active = new JCheckBox("Active");
	private JTextField value = new JTextField("", 6);
	private JButton save = new JButton("Save");

	/**
	 * Run the check
	 * 
	 * @param parameters
	 *            parameters for this check
	 * @param host
	 *            deliver analysis results to this host
	 * @return success of the check
	 */
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		this.host = host;
		this.parameters = parameters;
		Resource model = host.getAnalyzedModel();

		if (model.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Empty model"));
			return false;
		}

		if (model.getContents().get(0) instanceof Package) {
			Package content = (Package) model.getContents().get(0);
			List<Element> abac = UMLsecUtil.getStereotypedElements(content, UMLsec.ABAC);
			int abacNum = abac.size();
			if (abacNum == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Could not find a abac stereotype"));
				return false;
			}
			if (abacNum > 1) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Found " + abacNum
						+ " abac stereotypes, model should only contain one"));
				return false;
			}
			Element abacClass = abac.get(0);

			usersTag = UMLsecUtil.getStringValues("roles", UMLsec.ABAC, abacClass);
			if (usersTag.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				return false;
			}
			users = RABACCheck.parseTag(usersTag.get(0), null, 0);
			if (users.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Roles missing!"));
				return false;
			}

			List<Element> abacRequire = UMLsecUtil.getStereotypedElements(content, UMLsec.ABACREQUIRE);
			objects = new HashSet<String>();
			for (Element e : abacRequire) {
				objects.add(e instanceof Transition ? ((Transition) e).containingStateMachine().getName()
						: ((Operation) e).getClass_().getName());
			}

			List<Element> abacAttribute = UMLsecUtil.getStereotypedElements(content, UMLsec.ABACATTRIBUTE);
			for (Element e : abacAttribute) {
				List<String> nameTag = UMLsecUtil.getStringValues("name", UMLsec.ABACATTRIBUTE, e);
				// use name of operation when no explicit one is given
				String name = nameTag.size() == 0 ? ((NamedElement) e).getName() : nameTag.get(0);

				for (Attribute a : config.getAttributes()) {
					if (a.getName().equals(name)) {
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Duplicate attribute " + name
								+ " !"));
						return false;
					}
				}

				Attribute a = new Attribute();
				a.setName(name);
				config.getAttributes().add(a);
			}

			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "RABAC stereotypes are valid"));
			configGUI();
			return true;
		}

		host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Content is not a model!"));
		return false;
	}

	private void configGUI() {
		// build data structure to store session roles
		for (String u : users) {
			config.getSessions().put(u, new SetWrapper());
		}

		JPanel session = new JPanel();
		((FlowLayout) session.getLayout()).setAlignment(0);
		session.add(new JLabel("User:"));
		user = new JComboBox<String>(users.toArray(new String[0]));
		session.add(user);
		session.add(new JLabel("Role:"));
		role = new JComboBox<String>();
		session.add(role);
		active.setEnabled(false);
		session.add(active);

		JPanel filter = new JPanel();
		((FlowLayout) filter.getLayout()).setAlignment(0);
		filter.add(new JLabel("Attribute:"));
		attribute = new JComboBox<String>();
		for (Attribute a : config.getAttributes()) {
			attribute.addItem(a.getName());
		}
		filter.add(attribute);
		filter.add(new JLabel("Type:"));
		filter.add(type);
		filter.add(new JLabel("Object:"));
		object = new JComboBox<String>(objects.toArray(new String[0]));
		object.setEnabled(false);
		filter.add(object);
		filter.add(new JLabel("Value:"));
		filter.add(value);

		save.setAlignmentX(Component.CENTER_ALIGNMENT);

		user.addActionListener(this);
		role.addActionListener(this);
		active.addActionListener(this);
		attribute.addActionListener(this);
		type.addActionListener(this);
		object.addActionListener(this);
		value.addActionListener(this);
		save.addActionListener(this);

		JFrame configGUI = new JFrame();
		configGUI.setLayout(new BoxLayout(configGUI.getContentPane(), BoxLayout.Y_AXIS));
		configGUI.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		configGUI.setTitle("RABACsec transformation input");
		configGUI.setSize(600, 150);
		configGUI.setLocationRelativeTo(null);
		configGUI.add(session);
		configGUI.add(filter);
		configGUI.add(save);
		configGUI.setVisible(true);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		if (event.getSource() == user) {
			role.removeAllItems();
			role.setModel(new DefaultComboBoxModel<String>(RABACCheck.parseTag(usersTag.get(0),
					(String) user.getSelectedItem(), 0).toArray(new String[0])));
			role.setSelectedIndex(-1);
			active.setEnabled(false);
		}

		if (event.getSource() == role) {
			active.setEnabled(true);
			active.setSelected(config.getSessions().get((String) user.getSelectedItem()).getSet()
					.contains(role.getSelectedItem()));
		}

		if (event.getSource() == active) {
			Set<String> roles = config.getSessions().get((String) user.getSelectedItem()).getSet();
			if (active.isSelected()) {
				roles.add((String) role.getSelectedItem());
			} else {
				roles.remove((String) role.getSelectedItem());
			}
		}

		if (event.getSource() == attribute) {
			for (Attribute a : config.getAttributes()) {
				if (a.getName().equals(attribute.getSelectedItem())) {
					type.setSelectedItem(a.getName());
					value.setText(a.getValues().get(
							type.getSelectedItem().equals("User") ? user.getSelectedItem() : object.getSelectedItem()));
					break;
				}
			}
		}

		if (event.getSource() == type) {
			object.setEnabled(!type.getSelectedItem().equals("User"));
			updateAttributes();
		}

		if (event.getSource() == object) {
			for (Attribute a : config.getAttributes()) {
				if (a.getName().equals(attribute.getSelectedItem())) {
					value.setText(a.getValues().get(object.getSelectedItem()));
					break;
				}
			}
		}

		if (event.getSource() == value) {
			updateAttributes();
		}

		if (event.getSource() == save) {
			// reduce file size
			for (String u : users) {
				if (config.getSessions().get(u) != null && config.getSessions().get(u).getSet().isEmpty()) {
					config.getSessions().remove(u);
				}
			}

			try {
				Marshaller m = JAXBContext.newInstance(RABACConfig.class).createMarshaller();
				m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
				m.marshal(config,
						((OutputFileParameter) parameters.get("carisma.check.rabac.configuration")).getValue());
			} catch (Exception e) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Error writing configuration file!"));
			}
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Saved configuration file"));
		}
	}

	private void updateAttributes() {
		for (Attribute a : config.getAttributes()) {
			if (a.getName().equals(attribute.getSelectedItem())) {
				a.setType((String) type.getSelectedItem());
				a.getValues().put(
						(String) (type.getSelectedItem().equals("User") ? user.getSelectedItem()
								: object.getSelectedItem()), value.getText());
				break;
			}
		}
	}

}