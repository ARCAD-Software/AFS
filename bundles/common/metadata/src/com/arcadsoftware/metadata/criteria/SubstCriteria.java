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
package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Replace this criteria according to a given Code.
 * 
 * <p>
 * Default supported values for this code are references to Access rights :
 * 
 * <ul>
 * <li><b><i>entity</i>.create</b>: refer to the creation right of the given entity.
 * <li><b><i>entity</i>.update</b>: refer to the update right of the given entity.
 * <li><b><i>entity</i>.list</b>: refer to the list right of the given entity.
 * <li><b><i>entity</i>.read</b>: refer to the read right of the given entity.
 * <li><b><i>entity</i>.delete</b>: refer to the delete right of the given entity.
 * <li><b><i>entity</i>.<i>attribute</i>.update</b>: refer to the update right of the given entity's attribute.
 * <li><b><i>entity</i>.<i>attribute</i>.read</b>: refer to the read right of the given entity's attribute.
 * <li><b><i>entity</i>.<i>link</i>.create</b>: refer to the create/delete right of the given entity's association link.
 * <li><b><i>entity</i>.<i>link</i>.list</b>: refer to the delete right of the given entity's association link.
 * </ul>
 */
public class SubstCriteria extends AbstractSearchCriteria implements Cloneable {

	/**
	 * Substitute the given code with the given substCriteria
	 * 
	 * @param criteria
	 * @param code
	 * @param substCriteria
	 */
	public static void subst(ISearchCriteria criteria, String code, ISearchCriteria substCriteria) {
		if (code == null) {
			return;
		}
		if (criteria instanceof SubstCriteria) {
			if (code.equalsIgnoreCase(((SubstCriteria)criteria).getCode())) {
				((SubstCriteria)criteria).setCriteria(substCriteria);
			}
		} else if (criteria instanceof NotCriteria) {
			subst(((NotCriteria)criteria).getCriteria(), code, substCriteria);
		} else if (criteria instanceof AndCriteria) {
			for(ISearchCriteria c:((AndCriteria)criteria).getCriterias()) {
				subst(c, code, substCriteria);
			}
		} else if (criteria instanceof OrCriteria) {
			for(ISearchCriteria c:((OrCriteria)criteria).getCriterias()) {
				subst(c, code, substCriteria);
			}
		}
	}
	
	private String code;
	private transient ISearchCriteria criteria; 
	
	public SubstCriteria() {
		super();
	}
	
	public SubstCriteria(String code) {
		super();
		this.code = code;
	}
	
	public SubstCriteria(String code, ISearchCriteria criteria) {
		this(code);
		this.criteria = criteria;
	}

	private ISearchCriteria calcCriteria() {
		if ((code == null) || (code.indexOf('.') <= 0)) {
			return null;
		}
		String[] codes = code.split("."); //$NON-NLS-1$
		if (codes.length <= 1) {
			return null;
		}
		MetaDataEntity entity = MetaDataEntity.loadEntity(codes[0]);
		if (codes.length == 2) {
			if ("create".equals(codes[1])) { //$NON-NLS-1$
				return entity.getRightCreate();
			}
			if ("update".equals(codes[1])) { //$NON-NLS-1$
				return entity.getRightUpdate();
			}
			if ("delete".equals(codes[1])) { //$NON-NLS-1$
				return entity.getRightDelete();
			}
			if ("list".equals(codes[1])) { //$NON-NLS-1$
				return entity.getRightList();
			}
			if ("read".equals(codes[1])) { //$NON-NLS-1$
				return entity.getRightRead();
			}
			return null;
		}
		MetaDataAttribute att = entity.getAttribute(codes[1]);
		if (att != null) {
			if ("update".equals(codes[2])) { //$NON-NLS-1$
				return att.getRightUpdate(true);
			}
			if ("read".equals(codes[2])) { //$NON-NLS-1$
				return att.getRightRead(true);
			}
			return null;
		}
		MetaDataLink link = entity.getLink(codes[1]);
		if (link != null) {
			if ("create".equals(codes[2])) { //$NON-NLS-1$
				return link.getRightCreate(true);
			}
			if ("list".equals(codes[2])) { //$NON-NLS-1$
				return link.getRightList(true);
			}
			return null;
		}
		return null;
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if (criteria == null) {
			synchronized (this) {
				if (criteria == null) {
					criteria = calcCriteria();
					if (criteria == null) {
						return false;
					}
				}
			}
		}
		return criteria.test(bean, currentUser);
	}

	/**
	 * @param code the code to set
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * @return the code
	 */
	public String getCode() {
		return code;
	}

	/**
	 * @param criteria the substitute criteria to set
	 */
	public void setCriteria(ISearchCriteria criteria) {
		this.criteria = criteria;
	}

	/**
	 * @return the substitude criteria
	 */
	public ISearchCriteria getCriteria() {
		return criteria;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if (criteria == null) {
			criteria = calcCriteria();
			if (criteria == null) {
				return ConstantCriteria.FALSE;
			}
		}
		return criteria.reduce(context);
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new SubstCriteria(code, criteria);
	}

	@Override
	public String toString() {
		return Messages.Criteria_SubstituteWith + code + ']';
	}

}
