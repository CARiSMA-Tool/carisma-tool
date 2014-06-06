package carisma.evolution;

public class ChangeConstraint {
	private ConstraintType type = null;
	
	private Change referencedChange = null;
	
	private Change constrainedChange = null;
	
	public ChangeConstraint(
			final ConstraintType newType,
			final Change newReferencedChange,
			final Change newConstrainedChange) {
		if (newType != null && newReferencedChange != null && newConstrainedChange != null) {
			type = newType;
			referencedChange = newReferencedChange;
			constrainedChange = newConstrainedChange;
		}
	}
	
	public ConstraintType getType() {
		return type;
	}
	
	public Change getReferencedChange() {
		return referencedChange;
	}
	
	public Change getConstrainedChange() {
		return constrainedChange;
	}
	@Override
	public final String toString() {
		return constrainedChange.getRef() + "=" + type.toString() + "(" + referencedChange.getRef() + ")";
	}
	
	@Override
	public final boolean equals(final Object other) {
		if (other == null) {
			return false;
		}
		if (!(other instanceof ChangeConstraint)) {
			return false;
		}
		ChangeConstraint otherConstraint = (ChangeConstraint) other;
		if (otherConstraint.getType().equals(this.type) 
				&& otherConstraint.getConstrainedChange().equals(this.constrainedChange)
				&& otherConstraint.getReferencedChange().equals(this.referencedChange)) {
			return true;
		}
		return false;
	}
}
