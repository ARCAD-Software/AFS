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
import java.util.HashSet;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.Right;

/**
 * Test Access Right :
 * 
 * <ul>
 * <li> attribute = ""  test that the selected User entity possess the given right and param.
 * <li> attribute = "." test that the current user possess the given right and param.
 * <li> attibute = "anycode" test the attribute value is an User that possess the given right and param.
 * </ul>
 */
public class HasRightCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {
	
	private String attribute;
	private Integer right;
	private String param;
	private transient ReferenceLine attributeRef;
	
	public HasRightCriteria() {
		super();
	}

	/**
	 * @param attribute
	 * @param right
	 * @param param
	 */
	public HasRightCriteria(String attribute, Integer right, String param) {
		super();
		this.attribute = attribute;
		this.right = right;
		this.param = param;
	}

	/**
	 * @param attribute
	 * @param right
	 * @param param
	 */
	public HasRightCriteria(String attribute, Integer right, int param) {
		super();
		this.attribute = attribute;
		this.right = right;
		this.param = Integer.toString(param);
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
			if (param == null) {
				if (context.getCurrentUser().getProfile().hasRight(right)) {
					return ConstantCriteria.TRUE;
				}
			} else {
				try {
					int p = Integer.parseInt(param);
					if (right == null) {
						if (context.getCurrentUser().getProfile().hasRight(-1, p)) {
							return ConstantCriteria.TRUE;
						}
					} else if (context.getCurrentUser().getProfile().hasRight(right, p)) {
						return ConstantCriteria.TRUE;
					}
				} catch (NumberFormatException e) {
					Collection<Right> params;
					if (right == null) {
						params = context.getCurrentUser().getProfile().getParams();
					} else {
						params = context.getCurrentUser().getProfile().getParams(right);
					}
					if ((params == null) || params.isEmpty()) {
						return ConstantCriteria.FALSE;
					}
					HashSet<Integer> ids = new HashSet<>(params.size());
					for (Right r: params) {
						int p = r.getParam();
						if (p != 0) {
							ids.add(r.getParam());
						}
					}
					if (param.equals(".")) {
						return new InListCriteria(ids).reduce(context);
					}
					return new InListCriteria(param, ids).reduce(context);
				}
			}
			return ConstantCriteria.FALSE;
		}
		ReferenceLine attributeRef = context.getEntity().getAttributeLine(attribute);
		if (attributeRef != null) {
			context.useReference(attributeRef);
			if (param != null) {
				ReferenceLine paramRef = context.getEntity().getAttributeLine(param);
				if (paramRef == null) {
					return new HasRightCriteria(attribute, right, null);
				}
				context.useReference(paramRef);
			}
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

	public String getParam() {
		return param;
	}
		
	public ReferenceLine getAttributeRef() {
		return attributeRef;
	}

	public void setRight(Integer right) {
		this.right = right;
	}

	public void setParam(String param) {
		this.param = param;
	}

	public void setAttributeRef(ReferenceLine attributeRef) {
		this.attributeRef = attributeRef;
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((param == null) && (right == null)) {
			return true;
		}
		// The user to be tested...
		IConnectionUserBean user = null;
		if (attribute == null) {
			// Test the selected user.
			if (bean.getType().equals("user")) { //$NON-NLS-1$
				user = Activator.getInstance().getConnectionUser(bean.getId());
			}
		} else if (attribute.equals(".")) { //$NON-NLS-1$
			// Test the currently connected user (Immediate resolution).
			user = currentUser;
		} else {
			int attval = bean.getInt(attribute);
			if (attval > 0) {
				user = Activator.getInstance().getConnectionUser(bean.getId());
			}
		}
		if (user == null) {
			return false;
		}
		// Compute the parameter to be tested.
		int p = 0;
		if (param != null) {
			if (param.equals(".")) { //$NON-NLS-1$
				p = bean.getId();
			} else {
				try {
					p = Integer.parseInt(param);
				} catch (NumberFormatException e) {
					p = bean.getInt(param);
				}
			}
		}
		if (right == null) {
			return user.getProfile().hasRight(-1, p);
		}
		if (param == null) {
			return user.getProfile().hasRight(right);
		}
		return user.getProfile().hasRight(right, p);
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
			if (Activator.getInstance() != null) {
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
						sb.append(bm.get("name", right)); //$NON-NLS-1$
					}
				}
			} else {
				sb.append(right);
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
