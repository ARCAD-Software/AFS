package com.arcadsoftware.restful.connection.local;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.IMetaDataSelectionListener;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class LocalAuthSelectionListener implements IMetaDataSelectionListener {

	private final Activator activator;

	public LocalAuthSelectionListener(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public void onSelection(MetaDataEntity entity, BeanMapList selectedItems, IConnectionUserBean user,
			Language language) throws ResourceException {
		// TODO Auto-generated method stub

	}

}
