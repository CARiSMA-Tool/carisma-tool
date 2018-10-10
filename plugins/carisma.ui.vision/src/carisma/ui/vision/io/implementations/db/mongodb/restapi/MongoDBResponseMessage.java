package carisma.ui.vision.io.implementations.db.mongodb.restapi;


import carisma.core.io.implementations.db.ResponseMessage;

/**
 * Translates MongoDB http status codes into human readable messages.
 * 
 * @author speldszus
 *
 */
	public enum MongoDBResponseMessage implements ResponseMessage {
		/*
		 * Available messages and their http status codes.
		 */
		ClientError(400),
		CreatedDocument(201),
		Forbidden(403),
		InternalServerIssue(500),
		NoContent(204),
		NotFound(404),
		OK(200),
		SameContent(409),
		Unauthorized(401),
		UpdatedDocument(201);
		
		/**
		 * The http status code of this instance.
		 */
		private final int code;
		
		/**
		 * Creates a ModgoDBResponseMessage instance with the given status code.
		 * 
		 * @param statusCode A http status code
		 */
		private MongoDBResponseMessage(final int statusCode) {
			this.code = statusCode;
		}

		@Override
		public int getStatus() {
			return this.code;
		}
		
		/**
		 * Method for creating ResponseMessages from http status code
		 * and kind of executed operation on a MongoDB.
		 * 
		 * @param status The status code of the exuted request
		 * @param op The executed operation
		 * @return A ResonseMessage with human readable description
		 */
		public static ResponseMessage createFromHttpStatus(final int status, final Operation op) {
			switch(op) {
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
		
		/**
		 * Creates a ResponseMessage for a put operation.
		 * @param status The received status code
		 * @return A ResonseMessage with human readable description
		 */
		private static ResponseMessage getPutStatus(final int status) {
			if (status == UpdatedDocument.code) {
				return UpdatedDocument;
			} else if (status == ClientError.code) {
				return ClientError;
			} else if (status == Unauthorized.code) {
				return Unauthorized;
			} else if (status == Forbidden.code) {
				return Forbidden;
			} else if (status == SameContent.code) {
				return SameContent;
			} else if (status == NotFound.code) {
				return NotFound;
			} else if (status == InternalServerIssue.code) {
				return InternalServerIssue;
			} else {
				throw new RuntimeException("Unknown status code for http delete response: " + status);
			}
		}

		/**
		 * Creates a ResponseMessage for a post operation.
		 * @param status The received status code
		 * @return A ResonseMessage with human readable description
		 */
		private static ResponseMessage getPostStatus(final int status) {
			if (status == CreatedDocument.code) {
				return CreatedDocument;
			} else if (status == ClientError.code) {
				return ClientError;
			} else if (status == Unauthorized.code) {
				return Unauthorized;
			} else if (status == Forbidden.code) {
				return Forbidden;
			} else if (status == NotFound.code) {
				return NotFound;
			} else if (status == InternalServerIssue.code) {
				return InternalServerIssue;
			} else {
				throw new RuntimeException("Unknown status code for http delete response: " + status);
			}
		}

	/**
	 * Creates a ResponseMessage for a get operation.
	 * @param status The received status code
	 * @return A ResonseMessage with human readable description
	 */
	private static ResponseMessage getGetStatus(final int status) {
			if (status == OK.code) {
				return OK;
			} else if (status == ClientError.code) {
				return ClientError;
			} else if (status == Unauthorized.code) {
				return Unauthorized;
			} else if (status == Forbidden.code) {
				return Forbidden;
			} else if (status == NotFound.code) {
				return NotFound;
			} else if (status == InternalServerIssue.code) {
				return InternalServerIssue;
			} else {
				throw new RuntimeException("Unknown status code for http get response: " + status);
			}
		}

	/**
	 * Creates a ResponseMessage for a delete operation.
	 * @param status The received status code
	 * @return A ResonseMessage with human readable description
	 */
		private static ResponseMessage getDeleteStatus(final int status) {
			if (status == OK.code) {
				return OK;
			} else if (status == NoContent.code) {
				return NoContent;
			} else if (status == ClientError.code) {
				return ClientError;
			} else if (status == Unauthorized.code) {
				return Unauthorized;
			} else if (status == Forbidden.code) {
				return Forbidden;
			} else if (status == NotFound.code) {
				return NotFound;
			} else if (status == InternalServerIssue.code) {
				return InternalServerIssue;
			} else {
				throw new RuntimeException("Unknown status code for http delete response.");
			}
		}
		
		/**
		 * An enumeration with possible operations via a RestAPI on a MongoDB.
		 * 
		 * @author speldszus
		 */
		public enum Operation {
			delete,
			get,
			post,
			put
		}		
		
	}
