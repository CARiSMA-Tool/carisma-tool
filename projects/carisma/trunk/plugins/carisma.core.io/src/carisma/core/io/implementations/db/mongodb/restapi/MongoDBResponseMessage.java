package carisma.core.io.implementations.db.mongodb.restapi;

import static carisma.core.io.implementations.db.mongodb.restapi.MongoDBResponseMessage.Reason.*;

import carisma.core.io.implementations.db.ResponseMessage;

public class MongoDBResponseMessage implements ResponseMessage {

	private final int httpStatus;
	private final Reason reason;
	
	private MongoDBResponseMessage(int httpStatus, Reason reason){
		this.httpStatus = httpStatus;
		this.reason = reason;
	};
	
	public static ResponseMessage createFromHttpStatus(int status, Operation op){
		switch(op){
		case delete:
			return getDeleteStatus(status);
		case get:
			return getGetStatus(status);
		case post:
			return getPostStatus(status);
		case put:
			return getPutStatus(status);
		default:
			throw new RuntimeException("Unknow MongoDB Operation");
		
		}
	}
	
	private static ResponseMessage getPutStatus(int status) {
		Reason reason = null;
		if(status == 201){
			reason = UpdatedDocument;
		}
		else if(status == 400){
			reason = ClientError;
		}
		else if(status == 401){
			reason = Unauthorized;
		}
		else if(status == 403){
			reason = Forbidden;
		}
		else if(status == 409){
			reason = SameContent;
		}
		else if(status == 404){
			reason = NotFound;
		}
		else if(status == 500){
			reason = InternalServerIssue;
		}
		else{
			throw new RuntimeException("Unknown status code for http delete response.");
		}
		return new MongoDBResponseMessage(status, reason);
	}

	private static ResponseMessage getPostStatus(int status) {
		Reason reason = null;
		if(status == 201){
			reason = CreatedDocument;
		}
		else if(status == 400){
			reason = ClientError;
		}
		else if(status == 401){
			reason = Unauthorized;
		}
		else if(status == 403){
			reason = Forbidden;
		}
		else if(status == 404){
			reason = NotFound;
		}
		else if(status == 500){
			reason = InternalServerIssue;
		}
		else{
			throw new RuntimeException("Unknown status code for http delete response.");
		}
		return new MongoDBResponseMessage(status, reason);
	}

	private static ResponseMessage getGetStatus(int status) {
		Reason reason;
		if(status == 200){
			reason = OK;
		}
		else if(status == 400){
			reason = ClientError;
		}
		else if(status == 401){
			reason = Unauthorized;
		}
		else if(status == 403){
			reason = Forbidden;
		}
		else if(status == 404){
			reason = NotFound;
		}
		else if(status == 500){
			reason = InternalServerIssue;
		}
		else{
			throw new RuntimeException("Unknown status code for http get response: "+status);
		}
		return new MongoDBResponseMessage(status, reason);
	}

	private static ResponseMessage getDeleteStatus(int status) {
		Reason reason = null;
		if(status == 200){
			reason = OK;
		}
		else if(status == 204){
			reason = NoContent;
		}
		else if(status == 400){
			reason = ClientError;
		}
		else if(status == 401){
			reason = Unauthorized;
		}
		else if(status == 403){
			reason = Forbidden;
		}
		else if(status == 404){
			reason = NotFound;
		}
		else if(status == 500){
			reason = InternalServerIssue;
		}
		else{
			throw new RuntimeException("Unknown status code for http delete response.");
		}
		return new MongoDBResponseMessage(status, reason);
	}

	/* (non-Javadoc)
	 * @see carisma.core.io.implementations.db.mongodb.restapi.ResponseMessage#getStatus()
	 */
	@Override
	public int getStatus() {
		return httpStatus;
	}

	@Override
	public String toString() {
		return reason.toString();
	}
	
	public Reason getReason() {
		return reason;
	}

	public enum Operation {
		delete,
		get,
		post,
		put
	}
	
	public enum Reason {
		AlredaySameValue,
		ClientError,
		CreatedDocument,
		DocumentAlreadyExists,
		Forbidden,
		InternalServerIssue,
		NoContent,
		NotFound,
		OK,
		SameContent,
		Unauthorized,
		UpdatedDocument
	}
}
