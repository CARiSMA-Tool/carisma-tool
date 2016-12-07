package carisma.check.sequencediagramcrypto.message;


public class PeerImp implements Peer {
	
	final private String	peerName;
	
	public PeerImp(String peerName) {
		this.peerName = peerName;
	}
	
	@Override
	public String name() {
		return this.peerName;
	}
	
	// Auto-Generated
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((peerName == null) ? 0 : peerName.hashCode());
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
		if (!(obj instanceof PeerImp)) {
			return false;
		}
		PeerImp other = (PeerImp) obj;
		if (peerName == null) {
			if (other.peerName != null) {
				return false;
			}
		}
		else if (!peerName.equals(other.peerName)) {
			return false;
		}
		return true;
	}
	
}
