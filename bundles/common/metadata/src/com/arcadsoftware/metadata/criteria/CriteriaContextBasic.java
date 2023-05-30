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
package com.arcadsoftware.metadata.criteria;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This class implement a simple Context manager for Criteria reduction.  
 * 
 * <p>
 * This implementation need to be associated to :
 * 
 * <ul>
 * <li>A source Entity.
 * <li>A current user.
 * </ul>
 * 
 * <p>
 * The reduction process generate a list of used reference lines and association links.
 */
public class CriteriaContextBasic implements ICriteriaContext {

	private MetaDataEntity entity;
	private IConnectionUserBean currentUser;
	private HashMap<String, ReferenceLine> map;
	private HashMap<MetaDataLink, HashMap<String, ReferenceLine>> linksmap;
	
	public CriteriaContextBasic(MetaDataEntity entity, IConnectionUserBean currentUser) {
		super();
		this.entity = entity;
		this.currentUser = currentUser;
		map = new HashMap<String, ReferenceLine>();
		linksmap = new HashMap<MetaDataLink, HashMap<String,ReferenceLine>>();
	}

	public MetaDataEntity getEntity() {
		return entity;
	}

	public MetaDataEntity getEntity(String type) {
		return entity.getEntity(type);
	}

	public IConnectionUserBean getCurrentUser() {
		return currentUser;
	}

	public void useReference(ReferenceLine reference) {
		if (!reference.isEmpty()) {
			map.put(reference.getCode(), reference);
		}
	}

	public void useReferences(List<ReferenceLine> references) {
		for(ReferenceLine reference:references) {
			useReference(reference);
		}
	}

	public ReferenceLine getReference(String code) {
		return map.get(code);
	}
	
	public Collection<ReferenceLine> getReferences() {
		return map.values();
	}

	public void useLinkReference(MetaDataLink link, ReferenceLine reference) {
		HashMap<String, ReferenceLine> refs = linksmap.get(link);
		if (refs == null) {
			refs = new HashMap<String, ReferenceLine>();
			linksmap.put(link, refs);
		}
		if (reference != null){
			refs.put(reference.getCode(), reference);
		}
	}
	
	public Collection<MetaDataLink> getLinks() {
		return linksmap.keySet();
	}
	
	public Collection<ReferenceLine> getReferences(MetaDataLink link) {
		HashMap<String, ReferenceLine> refs = linksmap.get(link);
		if (refs == null) {
			return null;
		}
		return refs.values();
	}

	private boolean isRefsMultiMappers(Collection<ReferenceLine> refs) {
		for(ReferenceLine ref:refs) {
			if ((!entity.sameMapper(ref.getOriginEntity())) || (ref.getSecondMapperIndex() > 0)) {
				return true;
			}
		}
		return false;
	}

	public boolean isMapperUnique() {
		if (isRefsMultiMappers(map.values())) {
			return false;
		}
		for(Entry<MetaDataLink, HashMap<String, ReferenceLine>> e:linksmap.entrySet()) {
			if (isRefsMultiMappers(e.getValue().values())) {
				return false;
			}
			if (!entity.sameMapper(e.getKey().getRefEntity())) {
				return false;
			}
		}
		return true;
	}

	public ReferenceLine getReference(MetaDataLink link, String code) {
		HashMap<String, ReferenceLine> refs = linksmap.get(link);
		if (refs == null) {
			return null;
		}
		return refs.get(code);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(Messages.CriteriaContextBasic_Entity);
		sb.append(entity);
		sb.append(Messages.CriteriaContextBasic_CurrentUser);
		sb.append(currentUser);
		sb.append(Messages.CriteriaContextBasic_NumberOfReferences);
		sb.append(map.size());
		sb.append(Messages.CriteriaContextBasic_NumberOfLinks);
		sb.append(linksmap.size());
		return sb.toString();
	}
}
