package com.arcadsoftware.crypt;

/**
 * This Exception is a simple wraper around all security exception that may occurs during encryption operations.
 * 
 * @author ARCAD Software
 */
public class EncryptionError extends RuntimeException {

	private static final long serialVersionUID = -804600849257732396L;

	public EncryptionError() {
		super();
	}

	public EncryptionError(String message) {
		super(message);
	}

	public EncryptionError(Throwable cause) {
		super(cause);
	}

	public EncryptionError(String message, Throwable cause) {
		super(message, cause);
	}

	public EncryptionError(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
