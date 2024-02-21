/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
package com.arcadsoftware.metadata.rest;

import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.restlet.data.CharacterSet;
import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.data.Method;
import org.restlet.data.Reference;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.HTMLSimpleFormater;
import com.arcadsoftware.beanmap.xml.JSonBeanMapStream;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataLinkFormater;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.rest.internal.Activator;
import com.arcadsoftware.rest.UserLinkedIdentifiedResource;
import com.arcadsoftware.rest.UserLinkedResource;
import com.arcadsoftware.rest.JSONRepresentation;
import com.arcadsoftware.rest.XMLRepresentation;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class DataItemResource extends UserLinkedResource {

	public static final String ATTRIBUTE_CODE = "code"; //$NON-NLS-1$
	public static final String KEY_ID = UserLinkedIdentifiedResource.KEY_ID;
	
	private MetaDataEntity entity;
	private BeanMapList items;
	
	@Override
	protected void doInit() throws ResourceException {
		super.doInit();
		if (entity == null) {
			setEntity(getEntity(getType()));
			if ((entity == null) || (entity.getMapper() == null)) {
				setExisting(false);
				return;
			}
		}
		// Prise en charge multi-id ! 
		String id = (String) getRequest().getAttributes().get(KEY_ID);
		if (id != null) {
			String[] ids = id.split("\\+"); //$NON-NLS-1$
			items = new BeanMapList(ids.length);
			List<ReferenceLine> attrs = getPreloadedAttributes();
			if (attrs == null) {
				attrs = getSelectedAttributes(entity, false);
			}
			for (String value: ids) {
				if (value.length() > 0) {
					BeanMap bean = null;
					try {
						int i = Integer.parseInt(value);
						bean = getMapper().selection(entity, i, attrs, true);
					} catch (NumberFormatException e) {}
					if (bean != null) {
						items.add(bean);
					} else {
						items.addAll(findBeanListFromCode(Reference.decode(value)));
					}
				}
			}
			if (items.size() == 0) {
				setExisting(false);
				items = null;
				return;
			}
		}
		getAllowedMethods().add(Method.GET);
		getAllowedMethods().add(Method.PUT);
		getAllowedMethods().add(Method.DELETE);
		getAllowedMethods().add(Method.POST);
		addVariants(MEDIATYPES_BASESUP);
	}

	/**
	 * Get the specified entity from its type.
	 * 
	 * @param type
	 * @return null if this entity does not exists.
	 */
	protected MetaDataEntity getEntity(String type) {
		return MetaDataEntity.loadEntity(type);
	}

	/**
	 * Extender may override this method to load some attributes with the initial list of items.
	 * @return
	 */
	protected List<ReferenceLine> getPreloadedAttributes() {
		return null;
	}
	
	/**
	 * Return all values from the Form or, by default, into the Query.
	 * 
	 * @param form
	 * @param key
	 * @return
	 */
	protected String getColumns(Form form, String key) {
		String result = form.getValues(key, " ", true); //$NON-NLS-1$
		if (result == null) {
			result = getAttribute(key);
		}
		return result;
	}

	/**
	 * Get the list of attributes that may be returned within the result.
	 *
	 * <p>
	 * Test if the current user possess the "read" right on each selected attribute. If not the
	 * attributes is removed from the list.
	 * 
	 * @param entity the entity to get the attributes from. If null use the default entity.
	 * @param listable return only listable Attributes not all, if there is no attributes 
	 * @return
	 */
	protected List<ReferenceLine> getSelectedAttributes(MetaDataEntity entity, boolean listable) {
		if (entity == null) {
			entity = getEntity();
		}
		String atts = getColumns(getRequestForm(), "attributes"); //$NON-NLS-1$
		List<ReferenceLine> attributes;
		if (atts != null) {
			attributes = entity.getPublicAttributeLines(atts);
		} else if (listable) {
			attributes = entity.getListables();
		} else {
			attributes = entity.getAllPublicAttributes();
		}
		// Validate the selected attributes
		Iterator<ReferenceLine> itt = attributes.iterator();
		while(itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				// TODO Ce test devrait être complété par un test sur la totalité de la jointure !
				if (a.getRightRead(false) != null) {
					MetaDataEntity e = (MetaDataEntity) a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
					}
				}
			}
		}
		return attributes;
	}

	/**
	 * Change or initialize the entity value.
	 * 	
	 * <p>Call this method before the doInit() method to prevent the standard initialization.
	 * 
	 * @param entity
	 */
	protected void setEntity(MetaDataEntity entity) {
		this.entity = entity;
	}

	/**
	 * Change or initialize the list of selected items.
	 * 
	 * <p>Call this method before the doInit() method to prevent the standard initialization.
	 * 
	 * @param items
	 */
	protected void setItems(BeanMapList items) {
		this.items = items;
	}
	/**
	 * The selected items.
	 * 
	 * @return a list of BeanMap
	 */
	public BeanMapList getItems() {
		return items;
	}

	/**
	 * @return the type value of related Entity.
	 */
	protected String getType() {
		return getAttribute(DataParentResource.PARAMETER_TYPE);
	}
	
	/**
	 * @return the MetaDataEntity related to this resource call.
	 */
	protected MetaDataEntity getEntity() {
		return entity;
	}

	/**
	 * Get the Mapper associated with the Entity used by this resource.
	 * 
	 * @return
	 */
	protected IMapperService getMapper() {
		if (entity == null) {
			return null;
		}
		return entity.getMapper();
	}
	
	/**
	 * Load the first data Bean that possess the given value as a CODE attribute.
	 * @param value
	 * @return
	 * @see MetaDataEntity#METADATA_CODE
	 */
	protected BeanMap findBeanFromCode(String value) {
		MetaDataEntity entity = getEntity();
		if ((entity == null) || (entity.getMapper() == null)) {
			return null;
		}
		MetaDataAttribute att = getCodeAttribute(entity);
		if (att  == null) {
			return null;
		}
		return entity.getMapper().selectionFirst(entity, null, true, new ReferenceLine(att), value);
	}
	
	/**
	 * Load all data Bean that possess the given value as a CODE attribute.
	 * 
	 * @param value
	 * @return
	 * @see MetaDataEntity#METADATA_CODE
	 */
	protected BeanMapList findBeanListFromCode(String value) {
		MetaDataEntity entity = getEntity();
		if ((entity == null) || (entity.getMapper() == null)) {
			return new BeanMapList();
		}
		String code = entity.getMetadata().getString(MetaDataEntity.METADATA_CODE, ATTRIBUTE_CODE);
		if ((code == null) || (code.length() == 0) || (entity.getAttribute(code) == null)) {
			return new BeanMapList();
		}
		return entity.getMapper().selection(entity, null, true, code, value);
	}

	/**
	 * Retreive the MetadataAttribute that stand for a "textual identifier" for the given entity.
	 * 
	 * <p>
	 * By default this attribute is the 'code'.
	 * 
	 * @param entity
	 * @return null if there is no textual identifier for this entity.
	 * @see MetaDataEntity#METADATA_CODE
	 */
	protected MetaDataAttribute getCodeAttribute(MetaDataEntity entity) {
		String code = entity.getMetadata().getString(MetaDataEntity.METADATA_CODE, ATTRIBUTE_CODE);
		if ((code == null) || (code.length() == 0)) {
			return null;
		}
		return entity.getAttribute(code);
	}
	
	@Override
	protected Representation post(Representation entity, Variant variant) throws ResourceException {
		// post = put !!!
		return put(entity, variant);
	}

	/**
	 * Return true if the given "key" is an attribute of the HTTP request. 
	 * 
	 * <p> Such an attribute is passed through the given Form or into the Resource 
	 * path attributes (i.e. {part} of the resource path).  
	 * @param form
	 * @param key
	 * @return
	 */
	protected boolean isParameter(Form form,String key) {
		return ((form != null) && (form.getFirstValue(key) != null)) || //
				(getRequest().getResourceRef().getQueryAsForm().getFirstValue(key) != null) || //
				(getAttribute(key) != null);
	}

	/**
	 * Test the Delete right for a unique item.
	 * 
	 * @param entity
	 * @param item
	 * @param language
	 * @return
	 */
	protected boolean hasRightDelete(MetaDataEntity entity, BeanMap item, Language language) {
		return entity.getMapper().test(entity, item.getId(), entity.getRightDelete(), getUser()) &&
				Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREDELETE, entity, item, getUser(), language);
	}

	/**
	 * Test the Update right for an unique item.
	 * 
	 * @param entity The entity of the tested data.
	 * @param item The data item ID to test.
	 * @param language The current user language.
	 * @return
	 */
	protected boolean hasRightUpdate(MetaDataEntity entity, BeanMap item, Language language) {
		return hasRightUpdate(entity, item, false, false, language);
	}

	/**
	 * Test the Update right for an unique item.
	 * 
	 * @param entity The entity of the tested data.
	 * @param item The data item ID to test.
	 * @param deleted true if the operation may be executed on a deleted item !
	 * @param exists is true if we already know that this data exists and a check is not required.
	 * @param language The current user language.
	 * @return
	 */
	protected boolean hasRightUpdate(MetaDataEntity entity, BeanMap item, boolean deleted, boolean exists, Language language) {
		final ISearchCriteria right = entity.getRightUpdate();
		if (exists && ((right == null) || ConstantCriteria.TRUE.equals(right))) {
			return true;
		}
		return entity.getMapper().test(entity, item.getId(), right, deleted, getUser());
		// There is no Entity test (groovy) to perform here. It will be done on the updated attributes' values.
	}

	/**
	 * Test the Read right for a unique item.
	 * 
	 * <p>
	 * This method do not test Attributes read rights.
	 * @param entity The entity of the tested data.
	 * @param item The data item ID to test.
	 * @param language The current user language.
	 * @return
	 */
	protected boolean hasRightRead(MetaDataEntity entity, BeanMap item, Language language) {
		return entity.getMapper().test(entity, item.getId(), entity.getRightRead(), getUser());
	}
	
	/**
	 * Get a representation of the given result Object. This object should be a BeanMap or a BeanMapList
	 * but other java Object should work. The nature of the representation depends on the Variant type.
	 * 
	 * <p>
	 * Supported representation are:
	 * <ul>
	 * <li> HTML: only for BeanMap and BeanMapList.
	 * <li> JSON
	 * <li> XML
	 * </ul>
	 * <p> Default representation is XML.
	 * @param variant The client desired representation.
	 * @param form the Request Form. Used for presentation parameters.
	 * @param result The object to represent.
	 * @param language The client user language.
	 * @return A REST Representation.
	 */
	protected Representation getRepresentation(Variant variant, Form form, Object result, Language language, boolean subpage) {
		if (isHTML(variant)) {
			if (result instanceof BeanMapList) {
				return getHTMLRepresentation((BeanMapList) result,isParameter(form, "simple"), language, subpage); //$NON-NLS-1$
			}
			if (result instanceof BeanMap) {
				return getHTMLRepresentation((BeanMap) result,isParameter(form, "simple"), language, subpage); //$NON-NLS-1$
			}
		}
		if (isJSON(variant)) {
			return getJSONRepresentation(result, language);
		}
		return getXMLRepresentation(result, language);
	}

	/**
	 * Get an XML Representation of the given Object, using an XStream Parser.
	 * 
	 * @param object
	 * @param language
	 * @return
	 */
	protected Representation getXMLRepresentation(Object object, Language language) {
		return new XMLRepresentation(new XmlBeanMapStream().toXML(object), language);
	}

	/**
	 * Get an JSON Representation of the given Object, using an XStream Parser.
	 * 
	 * @param object
	 * @param language
	 * @return
	 */
	protected Representation getJSONRepresentation(Object object, Language language) {
		return new JSONRepresentation(new JSonBeanMapStream().toXML(object), language);
	}
	
	/**
	 * Get an HTML representation of a BeanMapList (a table... with link to data resources.)
	 * 
	 * @param list
	 * @param simple
	 * @param language
	 * @return
	 */
	protected Representation getHTMLRepresentation(BeanMapList list, boolean simple, Language language, boolean subpage) {
		final StringBuilder ret = new StringBuilder();
		List<String> segments = getRequest().getOriginalRef().getSegments();
		if (segments != null) {
			for (int i = segments.size() - 2; i >= 0; i--) {
				if ("data".equalsIgnoreCase(segments.get(i))) { //$NON-NLS-1$
					break;
				}
				ret.append("../"); //$NON-NLS-1$
			}
		}
		final String type;
		final MetaDataEntity entity;
		if (list.size() > 0) {
			type = list.get(0).getType();
			entity = MetaDataEntity.loadEntity(type);
		} else {
			type = getEntity().getType();
			entity = getEntity();
		}
		String title;
		if (list.isEmpty()) {
			title = String.format("%s Empty List", type);
		} else {
			title = String.format("%s List", type);
		}
		HTMLSimpleFormater f = new HTMLSimpleFormater(title) {
			@Override
			protected String getHRef(BeanMap bean, String key, String value) {
				ReferenceLine att = entity.getAttributeLine(key);
				if (att == null || att.isEmpty() || att.isFinal()) {
					return null;
				}
				return ret.toString() + att.getLastAttribute().getType() + '/' + value;
			}
			
			@Override
			protected String getHRef(BeanMap bean) {
				return ret.toString() + bean.getType() + '/' + bean.getId();
			}
		};
		f.append(list);
		return new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
	}

	/**
	 * Get an HTML representation of a BeanMap.
	 * 
	 * @param bean
	 * @param simple
	 * @param language
	 * @return
	 */
	protected Representation getHTMLRepresentation(BeanMap bean, boolean simple, Language language, boolean subpage) {
		final StringBuilder ret = new StringBuilder();
		List<String> segments = getRequest().getOriginalRef().getSegments();
		if (segments != null) {
			for (int i = segments.size() - 2; i >= 0; i--) {
				if ("data".equalsIgnoreCase(segments.get(i))) { //$NON-NLS-1$
					break;
				}
				ret.append("../"); //$NON-NLS-1$
			}
		}
		HTMLSimpleFormater f = new HTMLSimpleFormater(String.format("%s Data", getEntity().getType())) {
			@Override
			protected String getHRef(BeanMap bean, String key, String value) {
				ReferenceLine att = getEntity().getAttributeLine(key);
				if ((att == null) || att.isEmpty() || att.isFinal()) {
					return null;
				}
				return ret.toString() + att.getLastAttribute().getType() + '/' + value;
			}
		};
		f.append(bean);
		return new StringRepresentation(f.toString(), MediaType.APPLICATION_XHTML, language, CharacterSet.UTF_8);
	}
	
	/**
	 * Broadcast, asynchronously a message to the central User Action Log (if it is active !).
	 *  
	 * @param tag The metadata tag used to store the format message string.
	 * @param item
	 */
	protected void broadcastUserAction(String tag, BeanMap item) {
		MetaDataEntity entity;
		if (item.getType().equals(this.entity.getType())) {
			entity = this.entity;
		} else {
			entity = MetaDataEntity.loadEntity(item.getType());
		}
		Object broadcast = entity.getMetadata().get(tag);
		if (broadcast != null) {
			if (broadcast instanceof String) {
				broadcast = new MetaDataFormater((String) broadcast, entity);
				entity.getMetadata().put(tag, broadcast);
			}
			if (broadcast instanceof MetaDataFormater) {
				Hashtable<String, Object> props = new Hashtable<String, Object>();
				IConnectionUserBean user = getUser();
				props.put("uid", user.getId()); //$NON-NLS-1$
				if (user.getLogin() != null) {
					props.put("login", user.getLogin()); //$NON-NLS-1$
				}
				props.put("code", entity.getType()); //$NON-NLS-1$
				if (((MetaDataFormater) broadcast).getReferenceLines().isEmpty()) {
					props.put("message", ((MetaDataFormater) broadcast).getFormatString()); //$NON-NLS-1$
				} else if (((MetaDataFormater) broadcast).isComplete(item, false)) {
					props.put("message", ((MetaDataFormater) broadcast).format(item)); //$NON-NLS-1$
				} else {
					// We nedd to reload the item to complete the message text...
					BeanMap ritem = entity.getMapper().selection(entity, item.getId(), ((MetaDataFormater) broadcast).getReferenceLines(), true);
					if (ritem != null) {
						item = ritem;
					}
					props.put("message", ((MetaDataFormater) broadcast).format(item)); //$NON-NLS-1$
				}
				props.put("date", new Date()); //$NON-NLS-1$
				Activator.getInstance().fireBroadCastEvent(props);
			}
		}
	}

	protected void broadcastUserAction(MetaDataLink link, String tag, BeanMap source, BeanMap target) {
		Object broadcast = link.getMetadata().get(tag);
		if (broadcast != null) {
			if (broadcast instanceof String) {
				broadcast = new MetaDataLinkFormater((String) broadcast, link);
				link.getMetadata().put(tag, broadcast);
			}
			if (broadcast instanceof MetaDataLinkFormater) {
				Hashtable<String, Object> props = new Hashtable<String, Object>();
				IConnectionUserBean user = getUser();
				props.put("uid", user.getId()); //$NON-NLS-1$
				if (user.getLogin() != null) {
					props.put("login", user.getLogin()); //$NON-NLS-1$
				}
				props.put("code", link.getParent().getType()); //$NON-NLS-1$
				if (((MetaDataLinkFormater) broadcast).isStatic()) {
					props.put("message", ((MetaDataLinkFormater) broadcast).getFormatString()); //$NON-NLS-1$
				} else {
					if (!((MetaDataLinkFormater) broadcast).isSourceComplete(source)) {
						BeanMap item = link.getParent().getMapper().selection(link.getParent(), source.getId(), ((MetaDataLinkFormater) broadcast).getSourceReferenceLines(), true);
						if (item != null) {
							source = item;
						}
					}
					if (!((MetaDataLinkFormater) broadcast).isTargetComplete(target)) {
						BeanMap item = link.getRefEntity().getMapper().selection(link.getRefEntity(), target.getId(), ((MetaDataLinkFormater) broadcast).getTargetReferenceLines(), true);
						if (item != null) {
							target = item;
						}
					}
					props.put("message", ((MetaDataLinkFormater) broadcast).format(source, target)); //$NON-NLS-1$
				}
				props.put("date", new Date()); //$NON-NLS-1$
				Activator.getInstance().fireBroadCastEvent(props);
			}
		}
	}
}
