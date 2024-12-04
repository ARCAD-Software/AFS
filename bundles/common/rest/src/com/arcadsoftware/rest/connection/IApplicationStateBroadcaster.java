package com.arcadsoftware.rest.connection;

import org.restlet.data.Language;

public interface IApplicationStateBroadcaster {

	public ApplicationStatePlot broadcast(IConnectionUserBean user, Language language);
}
