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

	private final MetaDataEntity entity;
	private final IConnectionUserBean currentUser;
	private final HashMap<String, ReferenceLine> refmap;
	private final HashMap<String, List<MetaDataLink>> linksmap;
	private final HashMap<String, HashMap<String, ReferenceLine>> linksrefmap;
	
	public CriteriaContextBasic(MetaDataEntity entity, IConnectionUserBean currentUser) {
		super();
		this.entity = entity;
		this.currentUser = currentUser;
		refmap = new HashMap<String, ReferenceLine>();
		linksrefmap = new HashMap<String, HashMap<String, ReferenceLine>>();
		linksmap = new HashMap<String, List<MetaDataLink>>();
	}

	@Override
	public MetaDataEntity getEntity() {
		return entity;
	}

	@Override
	public MetaDataEntity getEntity(String type) {
		return entity.getEntity(type);
	}

	@Override
	public IConnectionUserBean getCurrentUser() {
		return currentUser;
	}

	@Override
	public void useReference(ReferenceLine reference) {
		if (!reference.isEmpty()) {
			refmap.put(reference.getCode(), reference);
		}
	}

	@Override
	public void useReferences(List<ReferenceLine> references) {
		for(ReferenceLine reference : references) {
			useReference(reference);
		}
	}

	@Override
	public ReferenceLine getReference(String code) {
		return refmap.get(code);
	}
	
	@Override
	public Collection<ReferenceLine> getReferences() {
		return refmap.values();
	}

	@Override
	public void useLinkReference(String code, ReferenceLine reference) {
		if (reference != null) {
			HashMap<String, ReferenceLine> refs = linksrefmap.get(code);
			if (refs == null) {
				refs = new HashMap<String, ReferenceLine>();
				linksrefmap.put(code, refs);
			}
			refs.put(reference.getCode(), reference);
		}
	}

	@Override
	public void useLinks(String code, List<MetaDataLink> links) {
		if ((links != null) && !links.isEmpty()) {
			linksmap.put(code, links);
		}
	}

	@Override
	public List<MetaDataLink> getLinks(String code) {
		return linksmap.get(code);
	}

	@Override
	public Collection<List<MetaDataLink>> getLinks() {
		return linksmap.values();
	}

	@Override
	public ReferenceLine getLinkReference(String linkCode, String code) {
		HashMap<String, ReferenceLine> linksref = linksrefmap.get(linkCode);
		if (linksref == null) {
			return null;
		}
		return linksref.get(code);
	}

	private boolean isRefsMultiMappers(Collection<ReferenceLine> refs) {
		for(ReferenceLine ref:refs) {
			if ((!entity.sameMapper(ref.getOriginEntity())) || (ref.getSecondMapperIndex() > 0)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean isMapperUnique() {
		if (isRefsMultiMappers(refmap.values())) {
			return false;
		}
		for(Entry<String, HashMap<String, ReferenceLine>> e: linksrefmap.entrySet()) {
			if (isRefsMultiMappers(e.getValue().values())) {
				return false;
			}
		}
		for (List<MetaDataLink> v: linksmap.values()) {
			for (MetaDataLink l : v) {
				if (!entity.sameMapper(l.getParent())) {
					return false;
				}
			}
		}
		return true;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(Messages.CriteriaContextBasic_Entity);
		sb.append(entity);
		sb.append(Messages.CriteriaContextBasic_CurrentUser);
		sb.append(currentUser);
		sb.append(Messages.CriteriaContextBasic_NumberOfReferences);
		sb.append(refmap.size());
		sb.append(Messages.CriteriaContextBasic_NumberOfLinks);
		sb.append(linksmap.size());
		return sb.toString();
	}

	@Override
	public boolean hasReferences() {
		if (!linksrefmap.isEmpty() || !linksmap.isEmpty()) {
			return true;
		}
		for (ReferenceLine ref: refmap.values()) {
			if (!ref.isSimple()) {
				return true;
			}
		}
		return false;
	}
}
