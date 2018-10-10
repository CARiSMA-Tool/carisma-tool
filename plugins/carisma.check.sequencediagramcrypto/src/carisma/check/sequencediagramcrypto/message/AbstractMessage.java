package carisma.check.sequencediagramcrypto.message;

import java.util.Collections;
import java.util.List;

public abstract class AbstractMessage implements Message {
	
	final private String			name;
	final private List<Argument>	arguments;
	final private Peer				source;
	final private Peer				target;
	final private int				msgNo;
	final private Guard				guard;
	final private boolean			symmetric;
	
	public AbstractMessage(String name, List<Argument> arguments, Peer source, Peer target, int msgNo,
			Guard guard, boolean symmetric) {
		this.name = name;
		this.arguments = Collections.unmodifiableList(arguments);
		this.source = source;
		this.target = target;
		this.msgNo = msgNo;
		this.guard = guard;
		this.symmetric = symmetric;
	}
	
	@Override
	public String name() {
		return this.name;
	}
	
	@Override
	public List<Argument> arguments() {
		return this.arguments;
	}
	
	@Override
	public boolean isAfter(Message message) {
		return this.msgNo > message.messageNumber();
	}
	
	@Override
	public boolean isBefore(Message message) {
		return this.msgNo < message.messageNumber();
	}
	
	@Override
	public Peer source() {
		return this.source;
	}
	
	@Override
	public boolean symmetric() {
		return this.symmetric;
	}
	
	@Override
	public Peer target() {
		return this.target;
	}
	
	@Override
	public int messageNumber() {
		return this.msgNo;
	}
	
	@Override
	public Guard guard() {
		return this.guard;
	}
	
	// Auto-Generated
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((arguments == null) ? 0 : arguments.hashCode());
		result = prime * result + msgNo;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((source == null) ? 0 : source.hashCode());
		result = prime * result + (symmetric ? 1231 : 1237);
		result = prime * result + ((target == null) ? 0 : target.hashCode());
		return result;
	}
	
	// Auto-Generated
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof AbstractMessage)) {
			return false;
		}
		AbstractMessage other = (AbstractMessage) obj;
		if (arguments == null) {
			if (other.arguments != null) {
				return false;
			}
		}
		else if (!arguments.equals(other.arguments)) {
			return false;
		}
		if (msgNo != other.msgNo) {
			return false;
		}
		if (name == null) {
			if (other.name != null) {
				return false;
			}
		}
		else if (!name.equals(other.name)) {
			return false;
		}
		if (source == null) {
			if (other.source != null) {
				return false;
			}
		}
		else if (!source.equals(other.source)) {
			return false;
		}
		if (symmetric != other.symmetric) {
			return false;
		}
		if (target == null) {
			if (other.target != null) {
				return false;
			}
		}
		else if (!target.equals(other.target)) {
			return false;
		}
		return true;
	}
	
	@Override
	public String toString() {
		return "AbsMessage [name=" + name + ", arguments=" + arguments + ", source=" + source
				+ ", target=" + target + ", msgNo=" + msgNo + ", guard=" + guard + ", symmetric="
				+ symmetric + "]";
	}
	
}
