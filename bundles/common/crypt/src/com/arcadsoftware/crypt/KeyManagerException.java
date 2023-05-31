package com.arcadsoftware.crypt;

public class KeyManagerException extends Exception {

	private static final long serialVersionUID = -4197977713878285497L;

	public KeyManagerException(String message) {
		super(message);
	}

	public KeyManagerException(Throwable cause) {
		super(cause.getLocalizedMessage(), cause);
	}

	public KeyManagerException(String message, Throwable cause) {
		super(message, cause);
	}

	public KeyManagerException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
