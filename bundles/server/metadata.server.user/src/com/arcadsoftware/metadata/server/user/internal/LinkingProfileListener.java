package com.arcadsoftware.metadata.server.user.internal;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class LinkingProfileListener implements IMetaDataLinkingListener {

	@Override
	public boolean testLink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	@Override
	public boolean testUnlink(MetaDataLink link, BeanMap sourceItem, BeanMap destItem, IConnectionUserBean user, Language language) throws ResourceException {
		if (Activator.UPDATEALLRIGHTPROFILE && Activator.TYPE_PROFILE.equals(sourceItem.getType()) && (sourceItem.getId() == 1)) {
			throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, "The Profile \"All Rights\" can not be modified.");
		}
		return true;
	}
}
