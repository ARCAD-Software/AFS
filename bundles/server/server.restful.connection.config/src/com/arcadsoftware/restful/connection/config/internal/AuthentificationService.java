/**
 * 
 */
package com.arcadsoftware.restful.connection.config.internal;

import java.util.ArrayList;
import java.util.List;

import org.restlet.Request;

import com.arcadsoftware.rest.connection.IBasicAuthentificationService;
import com.arcadsoftware.rest.connection.IConnectionCredential;

/**
 * Creation Date: 15 mars 2011
 */
public class AuthentificationService implements IBasicAuthentificationService {

	private final Activator activator;
	
	public AuthentificationService(Activator activator) {
		this.activator = activator;
	}

	@Override
	public void purgeConnectionCache() {}

	@Override
	public void purgeConnectionCache(int id) {}

	@Override
	public IConnectionCredential generateCredential(Request request, String identifier) {
		return activator.getConnectionCredential(identifier);
	}

	public List<String> getUserLogins(String userType, int userId) {
		return new ArrayList<String>();
	}

}
