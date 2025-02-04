package com.arcadsoftware.rest.connection;

import org.restlet.data.Language;

/**
 * OSGi Service 
 * 
 * @author ARCAD Software
 */
public interface IApplicationStateBroadcaster {

	public ApplicationStatePlot broadcast(IConnectionUserBean user, Language language);
}
