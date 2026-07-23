package com.arcadsoftware.crypt;

public class EncryptionException extends Exception {

	private static final long serialVersionUID = -962873520281058206L;

	public EncryptionException(String message) {
		super(message);
	}

	public EncryptionException(Throwable cause) {
		super(cause);
	}

	public EncryptionException(String message, Throwable cause) {
		super(message, cause);
	}

}
