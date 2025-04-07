package com.arcadsoftware.metadata.server.user.internal;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.IMetaDataUndeleteListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class UndeleteListener implements IMetaDataUndeleteListener {

	private final Activator activator;
	
	public UndeleteListener(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public boolean testUndeletion(MetaDataEntity entity, BeanMap item, IConnectionUserBean user, Language language)
			throws ResourceException {
		if (activator.isUserMaxlock() && (activator.getUserMax() > 0) && // User maximal number activated,
				Activator.TYPE_USER.equals(entity.getType()) && // and modified data is a user...
				(activator.getUserMax() <= entity.dataCount())) { // maximal number of undeleted user is reach
			activator.warn("Limit of maximal number of user reach ({} users).", activator.getUserMax());
			throw new ResourceException(Status.SERVER_ERROR_INSUFFICIENT_STORAGE, String.format("The current number of user declared in the application reach the fixed limitation of %d users.", activator.getUserMax()));
		}
		return true;
	}

	@Override
	public void postUndeletion(MetaDataEntity entity, BeanMap item, IConnectionUserBean user, Language language)
			throws ResourceException {}

}
