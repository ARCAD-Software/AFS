package com.arcadsoftware.crypt;

public class ConfiguredSSLContextException extends Exception {

	private static final long serialVersionUID = 5389889526706766502L;

	public ConfiguredSSLContextException() {
		super();
	}

	public ConfiguredSSLContextException(String message) {
		super(message);
	}

	public ConfiguredSSLContextException(Throwable cause) {
		super(cause.getLocalizedMessage(), cause);
	}

	public ConfiguredSSLContextException(String message, Throwable cause) {
		super(message, cause);
	}

	public ConfiguredSSLContextException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
