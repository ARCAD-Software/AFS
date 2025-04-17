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

import java.util.ArrayList;
import java.util.List;

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class MetaDataLink extends Element {

	private static final long serialVersionUID = 5140272887620505558L;

	private ISearchCriteria create;
	private ISearchCriteria list;

	/**
	 * Create an empty link element.
	 */
	public MetaDataLink() {
		super();
		setMetadata(new BeanMap());
	}

	/**
	 * Create a link element.
	 * 
	 * @param code
	 * @param referencedType
	 */
	public MetaDataLink(String code, String referencedType) {
		super(code, referencedType);
		setMetadata(new BeanMap());
	}

	/**
	 * Create an empty link element associated to the given entity.
	 * 
	 * @param parent
	 */
	public MetaDataLink(MetaDataEntity parent) {
		super(parent);
		setMetadata(new BeanMap());
	}

	/**
	 * Create a link element.
	 * 
	 * @param parent
	 * @param referencedType
	 */
	public MetaDataLink(MetaDataEntity parent, String referencedType) {
		super(parent, null, referencedType);
		setMetadata(new BeanMap());
	}

	/**
	 * Create a link element from a link model.
	 * 
	 * @param link
	 * @param parent
	 */
	public MetaDataLink(MetaDataLink link, MetaDataEntity parent) {
		super(link, parent);
		if (link.create != null) {
			create = link.create;
		}
		if (link.list != null) {
			list = link.list;
		}
	}

	/**
	 * Create a link element from a link model.
	 * 
	 * @param link
	 * @param parent
	 * @param type
	 */
	public MetaDataLink(MetaDataLink link, MetaDataEntity parent, String type) {
		this(link, parent);
		setType(type);
	}

	/**
	 * Duplicate a link element.
	 * 
	 * @param link
	 */
	public MetaDataLink(MetaDataLink link) {
		this(link, link.getParent());
	}

	/**
	 * Get the Creation right associated to this link (i.e. the right to add a link to the list).
	 * 
	 * <p>
	 * If this link does not define a creation right, the final access right can be deduced from
	 * the referenced entities. In this case if this link is a "reversed reference" link the
	 * creation is related to the referenced entity attribute Update right.  
	 * 
	 * @param inherited if true the right is deduced from referenced entities.
	 * @return A criteria or null if not right are required (if inherited is false).
	 * @see MetaDataAttribute#getRightUpdate(boolean)
	 */
	public ISearchCriteria getRightCreate(boolean inherited) {
		if (create != null) {
			return create;
		}
		if (!inherited) {
			return null;
		}
		String att = getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (att == null) {
			return ConstantCriteria.TRUE;
		}
		MetaDataEntity ref = getRefEntity();
		if (ref == null) {
			return ConstantCriteria.FALSE;
		}
		MetaDataAttribute a = ref.getAttribute(att);
		if (a == null) {
			return ConstantCriteria.FALSE;
		}
		return a.getRightUpdate(true);
	}

	public ISearchCriteria getRightList(boolean inherited) {
		if (list != null) {
			return list;
		}
		if (!inherited) {
			return null;
		}
		MetaDataEntity ref = getRefEntity();
		if (ref == null) {
			return ConstantCriteria.FALSE;
		}
		return ref.getRightList();
	}

	public void setCreate(ISearchCriteria create) {
		this.create = create;
	}

	public void setList(ISearchCriteria list) {
		this.list = list;
	}

	/**
	 * Get the referenced (destination) entity.
	 * 
	 * @return an entity or null if this entity is not declared (or can be loaded).
	 */
	public MetaDataEntity getRefEntity() {
		return getParent().getEntity(getType());
	}
	
	@Override
	public Object clone() throws CloneNotSupportedException {
		return new MetaDataLink(this);
	}

	/**
	 * Determine if this link associate two entities from the same mapper.
	 * @return true if the two linked entities belong to the same mapper.
	 */
	public boolean isLocal() {
		MetaDataEntity ref = getRefEntity();
		return (ref != null) && ref.sameMapper((MetaDataEntity) getParent());
	}

	/**
	 * @return true if this Link is not defined as an Hidden link (a link which can be modified by the Client but never returned to it).
	 */
	public boolean isPublic() {
		return !getMetadata().getBoolean(MetaDataEntity.METADATA_HIDDEN);
	}

	@Override
	public boolean isReadonly() {
		return super.isReadonly() || getMetadata().contains(MetaDataEntity.METADATA_COMBOLINK);
	}

	/**
	 * Link to data according to this link.
	 * 
	 * @param sourceid The ID of the data corresponding in the Source entity.
	 * @param destId The ID of the data corresponding in the Target entity.
	 * @return True if the two data are now linked.
	 */
	public boolean dataLinkTo(int sourceId, int destId) {
		if (isReadonly()) {
			return false;
		}
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkAdd(this, sourceId, destId);
	}

	/**
	 * Link to data according to this link.
	 * 
	 * <p>
	 * You have to pass the current user to the link creation if this link use the "pushUpdate" MetaData.
	 * 
	 * @param sourceid The ID of the data corresponding in the Source entity.
	 * @param destId The ID of the data corresponding in the Target entity.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return True if the two data are now linked.
	 */
	public boolean dataLinkTo(int sourceId, int destId, IConnectionUserBean currentUser) {
		if (isReadonly()) {
			return false;
		}
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkAdd(this, sourceId, destId, currentUser);
	}
	
	/**
	 * Test if a link exist between the two data.
	 * 
	 * @param sourceid The ID of the data corresponding in the Source entity.
	 * @param destId The ID of the data corresponding in the Target entity.
	 * @return True if the two data are linked.
	 */
	public boolean dataTest(int sourceId, int destId) {
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.linkTest(this, sourceId, destId);
	}

	/**
	 * Unlink two data.
	 * 
	 * @param sourceid The ID of the data corresponding in the Source entity.
	 * @param destId The ID of the data corresponding in the Target entity.
	 * @return True if the two data are now unlinked.
	 */
	public boolean dataUnlink(int sourceId, int destId) {
		if (isReadonly()) {
			return false;
		}
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.linkRemove(this, sourceId, destId);
	}

	/**
	 * Unlink two data.
	 * 
	 * <p>
	 * You have to pass the current user to the link creation if this link use the "pushUpdate" MetaData.
	 * 
	 * @param sourceid The ID of the data corresponding in the Source entity.
	 * @param destId The ID of the data corresponding in the Target entity.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return True if the two data are now unlinked.
	 */
	public boolean dataUnlink(int sourceId, int destId, IConnectionUserBean currentUser) {
		if (isReadonly()) {
			return false;
		}
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.linkRemove(this, sourceId, destId, currentUser);
	}
	
	/**
	 * Select all data linked to the given source.
	 *  
	 * @param sourceId
	 * @return
	 */
	public BeanMapList dataSelection(int sourceId) {
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.linkSelection(this, sourceId);
	}
	
	/**
	 * Count the number of data linked to the given source.
	 * 
	 * @param sourceId
	 * @return
	 */
	public int dataCount(int sourceId) {
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.linkCount(this, sourceId, false, null, false, null);
	}

	/**
	 * Translate the Attribute name to the given client user language.
	 * 
	 * <p>
	 * Try to translate the name of this attribute. If this translation is undefined then return the
	 * current value of the name or the code of this attribute.
	 * 
	 * <p>
	 * This method is not efficient to translate a whole MetaDataEntity. In most case it is useless
	 * on client side, because this id the MetadataEntity web-service responsibility to serve translated object.
	 * 
	 * @param language
	 * @return a non null String representation of this attribute.
	 * @see MetaDataEntity#clone(Language)
	 */
	public String getName(Language language) {
		String n = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, getParent().getType().replace('/', '.') + ".link." + getCode(), language); //$NON-NLS-1$
		if (n == null) {
			if (getName() != null) {
				return getName();
			}
			return getCode();
		}
		return n;
	}

	/**
	 * Return true if and only if the type of the parent entity is equal to the type of this link.
	 * 
	 * <p>
	 * The link is defining a recursive link on the same entity.
	 * 
	 * <p>
	 * Reverse links are never considered as recursive links, as the modification of the reference attribute does not avoid infinite loops.
	 * @return
	 */
	public boolean isRecursive() {
		return !getMetadata().getBoolean(MetaDataEntity.METADATA_IGNORERECURSIVITY) && //
				getParent().getType().equals(getType()) && //
				(getMetadata().get(MetaDataEntity.METADATA_REVERSELINK) == null) && //
				(getMetadata().get(MetaDataEntity.METADATA_COMBOLINK) == null);
	}
	
	/**
	 * If this link is a combined link then this method return the list of atomic links included in the chain.
	 * 
	 * <p>
	 * This method return this link if it is not a combined link.
	 * 
	 * @return null if the chain is broken. 
	 */
	public List<MetaDataLink> getLinkChain() {
		ArrayList<MetaDataLink> result = new ArrayList<MetaDataLink>(1);
		if (addAtomicLinks(result)) {
			return null;
		}
		if (result.isEmpty()) {
			return null;
		}
		MetaDataLink last = result.get(result.size() - 1);
		if (!getType().equals(last.getType())) {
			return null;
		}
		return result;
	}

	/**
	 * Add all the atomic (not combined) link relative to the given link.
	 * 
	 * @param result
	 * @param link
	 * @return true if a error occurs.
	 */
	protected boolean addAtomicLinks(List<MetaDataLink> result) {
		String combo = getMetadata().getString(MetaDataEntity.METADATA_COMBOLINK);
		if ((combo == null) || combo.isEmpty()) {
			result.add(this);
		} else {
			MetaDataEntity e = getParent();
			for (String code: combo.split(" ")) { //$NON-NLS-1$
				if (!code.isEmpty()) {
					MetaDataLink l = e.getLink(code);
					if (l == null) {
						return true;
					}
					if (l.addAtomicLinks(result)) {
						return true;
					}
					if (!result.isEmpty()) {
						e = result.get(result.size() - 1).getRefEntity();
					}
				}
			}
		}
		return false;
	}
}
