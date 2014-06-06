package carisma.regulatory.ontology.model;

public class RuleElementRelationModel {
    
    private String relationType = "";
    private String description = "";
    private String type = "";
    private String name = "";

    public RuleElementRelationModel(final String relationtype, 
            final String description, final String ruleElementType, final String ruleElementName) {
        this.setRelationType(relationtype);
        this.setDescription(description);
        this.setType(ruleElementType);
        this.setName(ruleElementName);
    }

    /**
     * @return the relationType
     */
    public String getRelationType() {
        return relationType;
    }

    /**
     * @param relationType the relationType to set
     */
    private void setRelationType(String relationType) {
        this.relationType = relationType;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description the description to set
     */
    private void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type the type to set
     */
    private void setType(String type) {
        this.type = type;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name the name to set
     */
    private void setName(String name) {
        this.name = name;
    }
}
