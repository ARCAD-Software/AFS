package com.arcadsoftware.rest.connection;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.security.Authenticator;

public class APIKeyAuthenticator extends Authenticator {

	// TODO Store the API key in the configuration.
	// TODO Add limitation date to the API-key.
	// TODO Retrieve a limitation date from an API key.
	// TODO Allow to revoke an API key.
	
	// TODO Store an API-Key be user ?
	
	public APIKeyAuthenticator(Context context) {
		super(context);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected boolean authenticate(Request request, Response response) {
		// TODO support BASIC key:...
		// TODO support API-KEY ...
		// TODO support cookie !?
		return false;
	}

}
