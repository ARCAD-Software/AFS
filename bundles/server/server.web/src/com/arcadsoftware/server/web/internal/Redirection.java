package com.arcadsoftware.server.web.internal;

import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;

public class Redirection extends Restlet {

	private final String path;
	
	public Redirection(Context context, String path) {
		super(context);
		this.path = path;
	}

	@Override
	public void handle(Request request, Response response) {
		response.redirectPermanent(path);
	}

	
}
