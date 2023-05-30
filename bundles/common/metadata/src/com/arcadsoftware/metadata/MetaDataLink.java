/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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

import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;

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
		return (ref != null) && ref.sameMapper((MetaDataEntity)getParent());
	}

	/**
	 * @return true if this Link is not defined as an Hidden link (a link which can be modified by the Client but never returned to it).
	 */
	public boolean isPublic() {
		return !getMetadata().getBoolean(MetaDataEntity.METADATA_HIDDEN);
	}

	/**
	 * Link to data according to this link.
	 * 
	 * @param sourceid
	 * @param destId
	 * @return
	 */
	public boolean dataLinkTo(int sourceId, int destId) {
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkAdd(this, sourceId, destId);
	}
	
	/**
	 * Test if a link exist between the two data.
	 * 
	 * @param sourceId
	 * @param destId
	 * @return
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
	 * @param sourceId
	 * @param destId
	 * @return
	 */
	public boolean dataUnlink(int sourceId, int destId) {
		IMapperService mapper = getParent().getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.linkRemove(this, sourceId, destId);
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
}
