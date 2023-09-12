package com.arcadsoftware.restful.internal;

public class ServerConfigurationException extends Exception {

	private static final long serialVersionUID = 647182112030301419L;

	public ServerConfigurationException() {
		super();
	}

	public ServerConfigurationException(String message) {
		super(message);
	}

	public ServerConfigurationException(Throwable cause) {
		super(cause);
	}

	public ServerConfigurationException(String message, Throwable cause) {
		super(message, cause);
	}

	public ServerConfigurationException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
