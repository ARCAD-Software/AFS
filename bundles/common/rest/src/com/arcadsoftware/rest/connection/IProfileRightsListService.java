package com.arcadsoftware.rest.connection;

import java.util.List;

import org.restlet.data.Language;

/**
 * OSGi service used to provide documentation about the Access Right used by the given profile.
 * 
 * @author ARCAD Software
 */
public interface IProfileRightsListService {

	/**
	 * Get a non null list of Right information objects.
	 * 
	 * @param profile
	 * @param language
	 * @return
	 */
	public List<RightInfo> getProfileRights(Profile profile, Language language);
}
