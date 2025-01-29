package carisma.check.idscheck.dto;

import org.eclipse.uml2.uml.Message;

import carisma.profile.umlsec.extension4ids.TransferType;

public class RelevantMessagesDto {
	private TransferType type;
	private Message transferRequest;
	private Message transferStart;
	private Message transferComplete;
	private Message transferTerminate;
	private Message transferSuspend;
	private Message pushPull;
	
	public RelevantMessagesDto(TransferType type, Message transferRequest, Message transferStart, Message transferComplete,
			Message transferTerminate, Message transferSuspend, Message pushPull) {
		this.type = type;
		this.transferRequest = transferRequest;
		this.transferStart = transferStart;
		this.transferComplete = transferComplete;
		this.transferTerminate = transferTerminate;
		this.transferSuspend = transferSuspend;
		this.pushPull = pushPull;
	}
	
	public TransferType getType() {
		return type;
	}
	public void setType(TransferType type) {
		this.type = type;
	}
	public Message getTransferRequest() {
		return transferRequest;
	}
	public void setTransferRequest(Message transferRequest) {
		this.transferRequest = transferRequest;
	}
	public Message getTransferStart() {
		return transferStart;
	}
	public void setTransferStart(Message transferStart) {
		this.transferStart = transferStart;
	}
	public Message getTransferComplete() {
		return transferComplete;
	}
	public void setTransferComplete(Message transferComplete) {
		this.transferComplete = transferComplete;
	}
	public Message getTransferTerminate() {
		return transferTerminate;
	}
	public void setTransferTerminate(Message transferTerminate) {
		this.transferTerminate = transferTerminate;
	}
	public Message getTransferSuspend() {
		return transferSuspend;
	}
	public void setTransferSuspend(Message transferSuspend) {
		this.transferSuspend = transferSuspend;
	}
	public Message getPushPull() {
		return pushPull;
	}
	public void setPushPull(Message pushPull) {
		this.pushPull = pushPull;
	}
	
	
}
