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

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test Access Right :
 * 
 * <ul>
 * <li> attribute = ""  test that the selected User entity possess the given right and param.
 * <li> attribute = "." test that the current user possess the given right and param.
 * <li> attibute = "anycode" test the attribute value is an User that possess the given right and param.
 * </ul>
 * 
 * TODO Implement paramAttribute to compare the param value to an attribute value.
 */
public class HasRightCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {
	
	private String attribute;
	private Integer right;
	private Integer param;
	private transient ReferenceLine attributeRef;
	
	public HasRightCriteria() {
		super();
	}

	/**
	 * @param attribute
	 * @param right
	 * @param param
	 */
	public HasRightCriteria(String attribute, Integer right, Integer param) {
		super();
		this.attribute = attribute;
		this.right = right;
		this.param = param;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((param == null) && (right == null)) {
			return ConstantCriteria.TRUE;
		}
		if (attribute == null) {
			// Test the selected user.
			if (!context.getEntity().getType().equals("user")) { //$NON-NLS-1$
				return ConstantCriteria.FALSE;
			}
			return this;
		}
		if (attribute.equals(".")) { //$NON-NLS-1$
			// Test the currently connected user (Immediate resolution).
			if (context.getCurrentUser() == null) {
				return ConstantCriteria.TRUE;
			}			
			if (right == null) {
				if (context.getCurrentUser().getProfile().hasRight(-1,param)) {
					return ConstantCriteria.TRUE;
				}
			} else if (param == null) {
				if (context.getCurrentUser().getProfile().hasRight(right)) {
					return ConstantCriteria.TRUE;
				}
			} else if (context.getCurrentUser().getProfile().hasRight(right,param)) {
				return ConstantCriteria.TRUE;
			}
			return ConstantCriteria.FALSE;
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			return this;
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new HasRightCriteria(attribute,right,param);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof HasRightCriteria) &&
			nullsOrEquals(attribute, ((HasRightCriteria)obj).attribute) &&
			nullsOrEquals(right, ((HasRightCriteria)obj).right) &&
			nullsOrEquals(param, ((HasRightCriteria)obj).param);
	}

	public ReferenceLine getAttributeReference() {
		return attributeRef;
	}
	
	public String getAttribute() {
		return attribute;
	}

	public Integer getRight() {
		return right;
	}

	public Integer getParam() {
		return param;
	}
		
	public ReferenceLine getAttributeRef() {
		return attributeRef;
	}

	public void setRight(Integer right) {
		this.right = right;
	}

	public void setParam(Integer param) {
		this.param = param;
	}

	public void setAttributeRef(ReferenceLine attributeRef) {
		this.attributeRef = attributeRef;
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((param == null) && (right == null)) {
			return true;
		}
		if (attribute == null) {
			// Test the selected user.
			if (bean.getType().equals("user")) { //$NON-NLS-1$
				IConnectionUserBean user = Activator.getInstance().getConnectionUser(bean.getId());
				if (user == null) {
					return false;
				}
				if (right == null) {
					return user.getProfile().hasRight(-1,param);
				}
				if (param == null) {
					return user.getProfile().hasRight(right);
				}
				return user.getProfile().hasRight(right,param);
			}
			return false;
		}
		if (attribute.equals(".")) { //$NON-NLS-1$
			// Test the currently connected user (Immediate resolution).
			if (currentUser == null) {
				return true;
			}			
			if (right == null) {
				return currentUser.getProfile().hasRight(-1,param);
			}
			if (param == null) {
				return currentUser.getProfile().hasRight(right);
			}
			return currentUser.getProfile().hasRight(right,param);
		}
		int attval = bean.getInt(attribute);
		if (right == null) {
			return currentUser.getProfile().hasRight(-1,attval);
		}
		return currentUser.getProfile().hasRight(right,attval);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute == null) {
			sb.append(Messages.Criteria_User);
		} else if (".".equals(attribute)) { //$NON-NLS-1$
			sb.append(Messages.Criteria_CurrentUser);
		} else {
			sb.append(attribute);
		}
		sb.append(Messages.Criteria_HasRight);
		if (right != null) {
			MetaDataEntity re = Activator.getInstance().getEntity("right"); //$NON-NLS-1$
			if (re == null) {
				sb.append(right);
			} else {
				MetaDataAttribute att = re.getAttribute("name"); //$NON-NLS-1$
				if (att == null) {
					sb.append(right);
				} else {
					BeanMap bm = new BeanMap("right", right); //$NON-NLS-1$
					att.translate(bm , Language.DEFAULT);
					sb.append(bm.get("name",right)); //$NON-NLS-1$
				}
			}
			if (param != null) {
				sb.append(" ["); //$NON-NLS-1$
			}
		}
		if (param != null) {
			sb.append(Messages.Criteria_WithParam);
			sb.append(param);
			if (right != null) {
				sb.append(']');
			}
		}
		return sb.toString();
	}

	public void setAttribute(String code) {
		attribute = code;
	}

}
