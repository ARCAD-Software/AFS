/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.metadata;

import java.util.List;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This OSGi service is a DAO interface to a dynamic data store. It serve BeanMaps based on Entity declaration.
 * 
 */
public interface IMapperService {

	/**
	 * This BeanMapper domain name.
	 */
	public static final String PROP_DOMAINNAME = "mapper.domain"; //$NON-NLS-1$

	/**
	 * OSGi service property. This property provide some information about the Mapper service functionality. This
	 * property is true if the service support soft deletion of items (and undeletion mechanisms).
	 * <p>
	 * Services that do not support soft deletion can ignore "harddelete" parameters and undeletion methods.
	 * 
	 */
	public static final String PROP_SUPPORT_SOFTDELETION = "softDeletion"; //$NON-NLS-1$

	/**
	 * OSGi service property. This property provide some information about the Mapper service functionality. This
	 * property is true if the service support selection pagination nativelly.
	 * <p>
	 * The Mappers that do not support pagination must still truncate selection result.
	 */
	public static final String PROP_SUPPORT_PAGINATION = "pagination"; //$NON-NLS-1$

	/**
	 * OSGi service property. This property provide some information about the Mapper service functionality. This
	 * property is true if the service support references to other mappers into references lines.
	 * <p>
	 * Services that do not support this feature are considered like terminals mapper. No entities associated to such
	 * mapper should declare references attributes to entities that belong to other mappers.
	 */
	public static final String PROP_SUPPORT_EXTRAREFERENCES = "extrareferences"; //$NON-NLS-1$

	/**
	 * OSGi service property. This property provide some information about the Mapper service functionality. This
	 * property is true if the service support milti link references into a same reference lines.
	 * <p>
	 * Multi-link reference may imply recurssives traitment that may not be possible for some mappers.
	 */
	public static final String PROP_SUPPORT_MULTILINKREFERENCES = "multilink"; //$NON-NLS-1$

	/**
	 * OSGi service property. This property provide some information about the Mapper service functionality. This
	 * property is true if the service support groups entities.
	 * <p>
	 * Mapper that do not implement groups facilities can ignore the group method. No entities associated to theses
	 * services should be delared as groups entities.
	 */
	public static final String PROP_SUPPORT_GROUPSENTITY = "groups"; //$NON-NLS-1$

	/**
	 * The OSGi service class identification.
	 * <p>
	 * Used for mapper from type "OGSI:[mappername]"
	 */
	public static final String clazz = IMapperService.class.getName();

	/**
	 * Indicate that this Mapper is replicated form another service.
	 */
	public static final String PROP_ALIAS = "alias"; //$NON-NLS-1$

	/**
	 * Called each time the Mapper is associated to a new domain.
	 * 
	 * @param domain
	 */
	public void addDomain(String domain);
	
	/**
	 * Return true if the given Entity come from a domain managed by this mapper.
	 * 
	 * @param entity
	 * @return
	 */
	public boolean sameDomain(MetaDataEntity entity);

	/**
	 * Return true if the given domain name is a domain managed by this mapper.
	 * 
	 * @param domain
	 * @return
	 */
	public boolean sameDomain(String domain);
	
	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param item
	 *            This BeanMap contains the information to store. Its Id will be forced to the created one.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(BeanMap item);

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * <p>
	 * This is the caller responsibility to ensure that the attributes list correspond to the values list.
	 * 
	 * @param type
	 *            the entity type of the element to be created
	 * @param attributes
	 *            the list of updated attributes codes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(String type, String attributes, List<Object> values);

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param entity
	 *            the entity of the element to be created
	 * @param attributes
	 *            the list of updated attributes codes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(MetaDataEntity entity, String attributes, List<Object> values);

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param entity
	 *            the entity of the element to be created
	 * @param attributes
	 *            the list of updated attributes codes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(MetaDataEntity entity, String attributes, Object... values);

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param attribute
	 *            the attribute that is updated
	 * @param values
	 *            the corresponding value
	 * @return the new item in a BeanMap with affected attributes and id or null if an error occurs.
	 */
	public BeanMap create(MetaDataAttribute attribute, Object value);

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * <p>
	 * This is the caller responsibility to ensure that the attributes list correspond to the values list.
	 * 
	 * @param attributes
	 *            the list of updated attributes, this list can <b>not</b> be null.
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(List<MetaDataAttribute> attributes, List<Object> values);


	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * <p>
	 * This is the caller responsibility to ensure that the attributes list correspond to the values list.
	 * 
	 * @param entity
	 *            The entity.
	 * @param attributes
	 *            the list of updated attributes.
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap create(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values);

	/**
	 * Delete an item from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE,PUT nor GET REST services.
	 * 
	 * @param item
	 *            The BeanMap object to delete.
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return true if the item exist and has been deleted.
	 * @see #undelete(BeanMap)
	 */
	public boolean delete(BeanMap item, boolean hardDelete);

	/**
	 * Delete an item from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE,PUT nor GET REST services.
	 * 
	 * @param type
	 *            the entity type of the element to be deleted
	 * @param itemId
	 *            the item id to delete
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return true if the item exist and has been deleted.
	 * @see #undelete(String, int)
	 */
	public boolean delete(String type, int itemId, boolean hardDelete);

	/**
	 * Delete an item from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE, PUT nor GET REST services.
	 * 
	 * @param entity
	 *            the entity of the element to be deleted
	 * @param itemId
	 *            the item id to delete
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return true if the item exist and has been deleted.
	 * @see #undelete(MetaDataEntity, int)
	 */
	public boolean delete(MetaDataEntity entity, int itemId, boolean hardDelete);

	/**
	 * Delete a list of items from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE, PUT nor GET REST services.
	 * 
	 * @param entity
	 *            the entity of the element to be deleted
	 * @param criteria
	 *            A conditional selection of item to delete.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return The number of deleted items.
	 */
	public int delete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete);
	
	/**
	 * Cancel a soft deletion operation of a list of items.
	 * 
	 * @param entity
	 *            the entity of the element to be deleted
	 * @param criteria
	 *            A conditional selection of item to undelete.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return The number of items updated.
	 */
	public int undelete(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser);
	
	/**
	 * Cancel a soft deletion operation.
	 * 
	 * <p>
	 * Hard deletion are not reversible.
	 * 
	 * @param item
	 *            The BeanMap object to undelete.
	 * @return True if the item has been successfully undeleted.
	 * @see #delete(BeanMap, boolean)
	 */
	public boolean undelete(BeanMap item);

	/**
	 * Cancel a soft deletion operation.
	 * 
	 * <p>
	 * Hard deletion are not reversible.
	 * 
	 * @param type
	 *            the entity type of the element to be undeleted
	 * @param itemId
	 *            the item id to undelete
	 * @return True if the item has been successfully undeleted.
	 * @see #delete(String, int, boolean)
	 */
	public boolean undelete(String type, int itemId);

	/**
	 * Cancel a soft deletion operation.
	 * 
	 * <p>
	 * Hard deletion are not reversible.
	 * 
	 * @param entity
	 *            the entity of the element to be undeleted
	 * @param itemId
	 *            the item id to undelete
	 * @return True if the item has been successfully undeleted.
	 * @see #delete(MetaDataEntity, int, boolean)
	 */
	public boolean undelete(MetaDataEntity entity, int itemId);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * <p>
	 * The whole content of the BeanMap object is going to be updated. So for optimization reasons it is preferable to
	 * store into this object only the modified information.
	 * 
	 * @param item
	 *            The BeanMap to update. Any attribute code used into this BeanMap will be updated into the storage.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(BeanMap item);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param type
	 *            The entity type to update
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(String type, int itemId, String attributes, List<Object> values);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param entity
	 *            The entity to update
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(MetaDataEntity entity, int itemId, String attributes, List<Object> values);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param entity
	 *            The entity to update
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(MetaDataEntity entity, int itemId, String attributes, Object... values);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attribute.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attribute
	 *            The attribute to be updated.
	 * @param value
	 *            The corresponding value.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(int itemId, MetaDataAttribute attribute, Object value);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The list of attributes to be updated, this list can <b>not</b> be null.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(int itemId, List<MetaDataAttribute> attributes, List<Object> values);

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The list of attributes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean update(MetaDataEntity entity, int itemId, List<MetaDataAttribute> attributes, List<Object> values);

	/**
	 * Update a set of items into the storage.
	 * 
	 * <p>
	 * Update operation can be applied to (soft) deleted items.
	 * 
	 * @param type
	 *            The entity type to update.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param criteria
	 *            A conditional selection of item to update.
	 * @return true if some of the items have been successfully updated.
	 */
	public boolean update(String type, String attributes, List<Object> values, String criteria);

	/**
	 * Update a set of items into the storage.
	 * 
	 * <p>
	 * Update operation can be applied to (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param type
	 *            The entity to update.
	 * @param attributes
	 *            The list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param criteria
	 *            A conditional selection of item to update.
	 * @return true if some of the items have been successfully updated.
	 */
	public boolean update(MetaDataEntity entity, String[] attributes, List<Object> values, String criteria);

	/**
	 * Update a set of items into the storage.
	 * 
	 * <p>
	 * Update operation can be applied to (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to be updated, this list can <b>not</b> be null.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param criteria
	 *            A conditional selection of item to update.
	 * @return true if some of the items have been successfully updated.
	 */
	public boolean update(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria);

	/**
	 * Update a set of items into the storage.
	 * 
	 * <p>
	 * Update operation can be applied to (soft) deleted items.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param entity
	 * @param attributes
	 *            The list of attributes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param criteria
	 *            A conditional selection of item to update.
	 * @return true if some of the items have been successfully updated.
	 */
	public boolean update(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria);

	/**
	 * Select an item from the storage.
	 * 
	 * @param item
	 *            the beanMap object to select.
	 * @param deleted
	 *            True if the item can be deleted.
	 * @return the selected item, or null if does not exist or is deleted (and <code>deleted</code> paramter is false).
	 */
	public BeanMap selection(BeanMap item, boolean deleted);

	/**
	 * Select an item from the storage.
	 * 
	 * @param entity
	 *            the entity to select.
	 * @param itemId
	 *            the item ID to select.
	 * @return the selected item, or null if does not exist or is deleted.
	 */
	public BeanMap selection(MetaDataEntity entity, int itemId);

	/**
	 * Select an item from the storage.
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @param itemId
	 *            the item ID to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the item can be deleted.
	 * @return the selected item, or null if does not exist or is deleted (and <code>deleted</code> paramter is false).
	 */
	public BeanMap selection(String type, int itemId, String attributes, boolean deleted);

	/**
	 * Select an item from the storage.
	 * 
	 * @param entity
	 *            the entity to select.
	 * @param itemId
	 *            the item ID to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the item can be deleted.
	 * @return the selected item, or null if does not exist or is deleted (and <code>deleted</code> paramter is false).
	 */
	public BeanMap selection(MetaDataEntity entity, int itemId, String attributes, boolean deleted);

	/**
	 * Select an item from the storage.
	 * 
	 * @param entity
	 *            the the entity to select.
	 * @param itemId
	 *            the item ID to select.
	 * @param attributes
	 *            The list of attributes to select. If null, all of the attributes values will be returned.
	 * @param deleted
	 *            True if the selected item can be deleted.
	 * @return the selected item, or null if does not exist or is deleted (and <code>deleted</code> paramter is false).
	 */
	public BeanMap selection(MetaDataEntity entity, int itemId, List<ReferenceLine> attributes, boolean deleted);

	/**
	 * Select all the items of a given entity.
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(String type);

	/**
	 * Select all the items of a given entity.
	 * 
	 * @param entity
	 *            the the entity to select.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity);

	/**
	 * Select all the item of a given entity.
	 * 
	 * @param entity
	 *            the the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity, String attributes);
	
	/**
	 * Select the items that possess the given attribute value.
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(String type, String attributes, boolean deleted, String attributeTest, Object value);

	/**
	 * Select the items that possess the given attribute value.
	 * 
	 * @param entity
	 *            the the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted, String attributeTest,
			Object value);

	/**
	 * Select the items that possess the given attribute value.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest,
			Object value);

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The, spaces separated, list of attributes codes used to sort the result. Theses attributes need to be
	 *            present into the selected attributes list. To process to a inverted sort prefix the code with an
	 *            explamation point.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(String type, String attributes, boolean deleted, String criteria, boolean distinct,
			String orders, IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to select, this list can <b>not</b> be null.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The list of attributes used to sort the result. Theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The list of attributes used to sort the result. Theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity, List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * <p>
	 * None attributes are returned with the result. 
	 * 
	 * @param entity
	 *            The entity to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria,
			boolean distinct, IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * @param entity
	 *            the the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The, spaces separated, list of attributes codes used to sort the result. theses attributes need to be
	 *            present into the selected attributes list. To process to a inverted sort prefix the code with an
	 *            explamation point.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, String orders, IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Count the number of items of the specified entity type. Deleted items are ignored.
	 * 
	 * @param type
	 *            the type of the entity to count.
	 * @return the number of items selected.
	 */
	public int count(String type);

	/**
	 * Count the number of items of the specified entity. Deleted items are ignored.
	 * 
	 * @param entity
	 *            the entity to count.
	 * @return the number of items selected.
	 */
	public int count(MetaDataEntity entity);

	/**
	 * Count the number of selected items that possess the given attribute value.
	 * 
	 * @param type
	 *            the type of the entity to count.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the number of items selected.
	 */
	public int count(String type, boolean deleted, String attributeTest, Object value);

	/**
	 * Count the number of selected items that possess the given attribute value.
	 * 
	 * @param entity
	 *            the entity to count.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the number of items selected.
	 */
	public int count(MetaDataEntity entity, boolean deleted, ReferenceLine attributeTest, Object value);

	/**
	 * Count the number of selected items that correspond to the given condition.
	 * 
	 * @param type
	 *            the type of the entity to count.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be counted (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of items selected.
	 */
	public int count(String type, boolean deleted, String criteria, boolean distinct, IConnectionUserBean currentUser);

	/**
	 * Count the number of selected items that correspond to the given condition.
	 * 
	 * @param entity
	 *            the entity to count.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be counted (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of items selected.
	 */
	public int count(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser);

	/**
	 * Test a complex condition and return true if it succeed.
	 * 
	 * @param entity
	 *            the entity to test.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the selection return at least one item.
	 */
	public boolean test(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser);

	/**
	 * Test a complex condition against a specific item and return true if it succeed.
	 * 
	 * @param item
	 *            The BeanMap object to test.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the condition is applicable to the given item.
	 */
	public boolean test(BeanMap item, ISearchCriteria criteria, IConnectionUserBean currentUser);

	/**
	 * Test a complex condition against a specific item and return true if it succeed.
	 * 
	 * @param entity
	 *            the entity to count.
	 * @param itemId
	 *            The item ID to test.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the condition is applicable to the given item.
	 */
	public boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, IConnectionUserBean currentUser);

	/**
	 * Test a complex condition against a specific item and return true if it succeed.
	 * 
	 * @param entity
	 *            the entity to count.
	 * @param itemId
	 *            The item ID to test.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param deleted
	 *            The test must be performed on a deleted element
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the condition is applicable to the given item.
	 */
	public boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, boolean deleted, IConnectionUserBean currentUser);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(String type, String attributes, boolean deleted, String attributeTest, Object value);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param entity
	 *            the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted, String attributeTest,
			Object value);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param entity
	 *            the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted,
			ReferenceLine attributeTest, Object value);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest,
			Object value);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param type
	 *            the type of the entity to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(String type, String attributes, boolean deleted, String criteria,
			IConnectionUserBean currentUser);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 * 
	 * @param attributes
	 *            The list of attributes to select, this list can <b>not</b> be null.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			IConnectionUserBean currentUser);

	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * <p>
	 * The entity to update is deducted from the parent entity of the given attributes.
	 *
	 * @param entity
	 *            The entity selected.
	 * @param attributes
	 *            The list of attributes to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the first result of the selection.
	 */
	public BeanMap selectionFirst(MetaDataEntity entity, List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			IConnectionUserBean currentUser);

	/**
	 * Add a link between two items. A link is an oriented relation that have an origin and a target. Some link can be
	 * reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param source
	 *            The BeanMap object that is the origin of the link.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link has been created.
	 */
	public boolean linkAdd(BeanMap source, String linkCode, int destId);

	/**
	 * Add a link between two items. A link is an oriented relation that have an origin and a target. Some link can be
	 * reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param source
	 *            The BeanMap object that is the origin of the link.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param dest
	 *            The BeanMap object that is the target of the link.
	 * @return true if the link has been created.
	 */
	public boolean linkAdd(BeanMap source, String linkCode, BeanMap dest);

	/**
	 * Add a link between two items. A link is an oriented relation that have an origin and a target. Some link can be
	 * reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link has been created.
	 */
	public boolean linkAdd(String sourceType, String linkCode, int sourceId, int destId);

	/**
	 * Add a link between two items. A link is an oriented relation that have an origin and a target. Some link can be
	 * reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param link
	 *            The link metadata definition.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link has been created.
	 */
	public boolean linkAdd(MetaDataLink link, int sourceId, int destId);

	/**
	 * Test if two item are linked with each other.
	 * 
	 * <p>
	 * The link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link exists.
	 */
	public boolean linkTest(String sourceType, String linkCode, int sourceId, int destId);

	/**
	 * Test if two item are linked with each other.
	 * 
	 * <p>
	 * The link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param link
	 *            The link metadata definition.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link exists.
	 */
	public boolean linkTest(MetaDataLink link, int sourceId, int destId);

	/**
	 * Test if two item are linked with each other.
	 * 
	 * <p>
	 * The link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param link
	 *            The link metadata definition.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @param ignoreSubdivision
	 *            If false and if the link parent entity possess a recursive link then all the elements linked to 
	 *            the sourceId plus all elements linked to any other source item linked, recursivelly to the
	 *            sourceID one, will be tested.
	 * @return true if the link exists.
	 */
	public boolean linkTest(MetaDataLink link, int sourceId, int destId, boolean ignoreSubdivision);


	/**
	 * Perform a link test across a chain of multiple links.
	 * 
	 * <p>
	 * The link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param links
	 * @param sourceId
	 * @param destId
	 * @param ignoreSubdivision
	 * @return
	 */
	public boolean linkTest(List<MetaDataLink> links, int sourceId, int destId, boolean ignoreSubdivision);

	/**
	 * Remove a link between two items. A link is an oriented relation that have an origin and a target. Some link can
	 * be reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link has been removed.
	 */
	public boolean linkRemove(String sourceType, String linkCode, int sourceId, int destId);

	/**
	 * Add a link between two items. A link is an oriented relation that have an origin and a target. Some link can be
	 * reversed, but both must be declared.
	 * 
	 * <p>
	 * This link can be either as association (n-to-m relation) or a reversed reference (n-to-1 relation).
	 * 
	 * @param link
	 *            The link metadata definition.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param destId
	 *            the item id target of the link.
	 * @return true if the link has been removed.
	 */
	public boolean linkRemove(MetaDataLink link, int sourceId, int destId);

	/**
	 * Return the list of target items linked to the specified source item.
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId);

	/**
	 * Return the list of target items linked to the specified source item.
	 * 
	 * @param link
	 *            The link object.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(MetaDataLink link, int sourceId);

	/**
	 * Return the list of target items linked to the specified source item that possess the given attribute value.
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String attributeTest, Object value);

	/**
	 * Return the list of target items linked to the specified source item that possess the given attribute value.
	 * 
	 * @param link
	 *            The link meta-data object.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, String attributes, boolean deleted,
			String attributeTest, Object value);

	/**
	 * Return the list of target items linked to the specified source item that possess the given attribute value.
	 * 
	 * @param link
	 *            The link meta-data object.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The list of attributes to select. If null, all of the attributes values will be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ReferenceLine attributeTest, Object value);

	/**
	 * Return the list of target items linked to the specified source item that possess the given attribute value.
	 * 
	 * @param links
	 *            A chain of link meta-data objects.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The list of attributes to select. If null, all of the attributes values will be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param ignoreSubdivision
	 *            If false and if the link parent entity possess a recursive link then all the elements linked to 
	 *            the sourceId plus all elements linked to any other source item linked, recursivelly to the
	 *            sourceID one, will be returned.
	 * @param attributeTest
	 *            The attribute to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			boolean ignoreSubdivision, ReferenceLine attributeTest, Object value);

	/**
	 * Return the list of target items linked to the specified source item that correspond to the given complex
	 * condition.
	 * 
	 * @param sourceType
	 *            The type of the origin entity.
	 * @param linkCode
	 *            The link code defined for the origin entity.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The, spaces separated, list of attributes codes used to sort the result. theses attributes need to be
	 *            present into the selected attributes list. To process to a inverted sort prefix the code with an
	 *            explamation point.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String criteria, boolean distinct, String orders, IConnectionUserBean currentUser,
			int page, int limit);

	/**
	 * Return the list of target items linked to the specified source item that correspond to the given complex
	 * condition.
	 * 
	 * @param link
	 *            The link meta-data object.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The list of attributes to select. If null, all of the attributes values will be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The list of attributes used to sort the result. theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort, flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser,
			int page, int limit);

	/**
	 * Return the list of target items linked to the specified source item that correspond to the given complex
	 * condition.
	 * 
	 * @param links
	 *            A chain of link meta-data objects.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param attributes
	 *            The list of attributes to select. If null, all of the attributes values will be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param ignoreSubdivision
	 *            If false and if the link parent entity possess a recursive link then all the elements linked to 
	 *            the sourceId plus all elements linked to any other source item linked, recursivelly to the
	 *            sourceID one, will be returned.
	 * @param orders
	 *            The list of attributes used to sort the result. theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort, flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of linked item. an empty list is returned if no linked item is found.
	 */
	public BeanMapList linkSelection(List<MetaDataLink> links, int sourceId, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, boolean ignoreSubdivision, List<ReferenceLine> orders, 
			IConnectionUserBean currentUser, int page, int limit);

	/**
	 * Count the selected target items linked to the specified source item that correspond to the given complex
	 * condition.
	 * 
	 * @param link
	 *            The link meta-data object.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of selected items.
	 */
	public int linkCount(MetaDataLink link, int sourceId, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser);

	/**
	 * Count the selected target items linked to the specified source item that correspond to the given complex
	 * condition.
	 * 
	 * @param links
	 *            A chain of link meta-data objects.
	 * @param sourceId
	 *            the item id origin of the link.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            If true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param ignoreSubdivision
	 *            If false and if the link parent entity possess a recursive link then all the elements linked to 
	 *            the sourceId plus all elements linked to any other source item linked, recursivelly to the
	 *            sourceID one, will be returned.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of selected items.
	 */
	public int linkCount(List<MetaDataLink> links, int sourceId, boolean deleted, ISearchCriteria criteria, boolean distinct, boolean ignoreSubdivision,
			IConnectionUserBean currentUser);

}