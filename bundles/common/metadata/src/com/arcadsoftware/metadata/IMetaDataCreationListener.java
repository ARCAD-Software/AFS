package com.arcadsoftware.metadata;

import java.util.List;

import org.restlet.data.Language;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Listener (synchronized) of items modifications. 
 *
 * <p>
 * This listener is a specialization of the {@link IMetaDataModifyListener}, only called for the creation of a new data.
 * When a new data in created both IMetaDataCreationListener and IMetaDataModifyListener are called. The IMetaDataCreationListener are
 * called first. 
 * 
 * @author ARCAD Software
 */
public interface IMetaDataCreationListener {
	
	/**
	 * This property define the listened entity type.
	 */
	public static final String PROP_TYPE = IMetaDataSelectionListener.PROP_TYPE;
	
	/**
	 * Called before modification or creation (originalItem is null).
	 * 
	 * <p>
	 * Use this method to test, change or complete the modified attributes of the entity. Any modification to modifiedItem will be recorded.
	 * 
	 * <p>
	 * Default implementation return true. Returning false will cancel the operation.
	 * 
	 * @param entity The corresponding entity.
	 * @param requestedItem the values of the object to requested to store.
	 * @param attributes the attributes list used in the requestedItem.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @return false if the process should be stopped.
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public default boolean testCreation(MetaDataEntity entity, BeanMap requestedItem, List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException {
		return true;
	}

	/**
	 * Called just after the item modification or creation (originalItem is null).
	 * 
	 * @param entity The corresponding entity.
	 * @param requestedItem the values of the object to requested to store.
	 * @param createdItem the attributes values as stored in the database and to return to the client.
	 * @param attributes the modified attributes list.
	 * @param user the current user (may be null).
	 * @param language The current user language (or default language).
	 * @throws ResourceException throw this exception to return a specific error message to the client.
	 */
	public void postCreation(MetaDataEntity entity, BeanMap requestedItem, BeanMap createdItem, List<MetaDataAttribute> attributes, IConnectionUserBean user, Language language) throws ResourceException;

}
