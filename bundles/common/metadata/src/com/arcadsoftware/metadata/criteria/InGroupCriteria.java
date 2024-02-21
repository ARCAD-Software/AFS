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
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Test that <code>item</code> is in <code>group</code> (<code>type</code> is the group type).
 * 
 * <p>
 * <code>item</code> can be :
 * <ul>
 * <li>null, then the test refer to the currently selected item.
 * <li>a fixed integer id, then the test refer to this id.
 * <li>an attribute reference, then the test refer to the current value of this attribute.
 * </ul>
 * <p>
 * <code>group</code> can be :
 * <ul>
 * <li>a fixed integer id, then the test refer to this group id. In this case a <code>type</code> must be specified to define the type of the group entity.
 * <li>an attribute reference, then the test refer to the group designed by the current value of this attribute.
 * </ul>
 * 
 * <p>
 * For instance : 
 * <br/>(item="contact",group="maingroup") test that the select values possess an attribute value "contact" that belong to the group value of the attribute "maingroup").
 * <br/>(group="owner.membergroup") test that the currently selected value is a member of the specified "owner.membergroups" group.
 * <br/>(group=12,type="usersgroup") test that the currently selected value is a member of the 12st "usersgroup".
 * <br/>(item=1,group="associatedgroup") test that group specified by the attribute "associatedgroup" contains the item id 1.
 */
public class InGroupCriteria extends AbstractSearchCriteria implements Cloneable {

	private String item;
	private String group;
	private String type;
	
	/**
	 * Default constructor
	 */
	public InGroupCriteria() {
		super();
	}

	/**
	 * Test that 'item' is in 'group'. 'type' is the group type.
	 * @param item the item to test, null to test currently selected item, a fixed id value or an attribute code.
	 * @param group the group id, an fixed value or an attribute code.
	 * @param type the group type entity (if group is an fixed value).
	 */
	public InGroupCriteria(String item, String group,String type) {
		super();
		this.item = item;
		this.group = group;
		this.type = type;
	}
	
	private int isInt(String value) {
		if (value == null) {
			return 0;
		}
		try {
			return Integer.parseInt(value);
		} catch (NumberFormatException e) {
			return 0;
		}
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		if ((item == null) || (group == null)) {
			return ConstantCriteria.FALSE;
		}
		String itemType = context.getEntity().getType();
		ReferenceLine itemRef = null;
		if (isInt(item) == 0) {
			itemRef = context.getEntity().getAttributeLine(item);
			if ((itemRef == null) || itemRef.isEmpty()) {
				return ConstantCriteria.FALSE;
			}
			itemType = itemRef.getLast().getType();
		} else if (item == null) {
			itemType = context.getEntity().getType();
		}
		ReferenceLine groupRef = null;
		if (isInt(group) == 0) {
			groupRef = context.getEntity().getAttributeLine(group);
			if ((groupRef == null) || groupRef.isEmpty()) {
				return ConstantCriteria.FALSE;
			}
			MetaDataEntity entity = groupRef.getLastAttribute().getRefEntity();
			if ((entity == null) || (!entity.isGroup()) || ((itemType != null) && !itemType.equals(entity.getGroupType()))) {
				return ConstantCriteria.FALSE;
			}
		} else {
			if (type == null) {
				return ConstantCriteria.FALSE;
			}
			MetaDataEntity entity = context.getEntity(type);
			if ((entity == null) || (!entity.isGroup()) || ((itemType != null) && !itemType.equals(entity.getGroupType()))) {
				return ConstantCriteria.FALSE;
			}
		}
		if (itemRef != null) {
			context.useReference(itemRef);
		}
		if (groupRef != null) {
			context.useReference(groupRef);
		}
		return this;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new InGroupCriteria(item,group,type);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof InGroupCriteria) &&
			nullsOrEquals(item,((InGroupCriteria)obj).item) &&
			nullsOrEquals(group,((InGroupCriteria)obj).group) &&
			nullsOrEquals(type,((InGroupCriteria)obj).type);
	}

	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		// TODO Implémenter un test d'appartenance à un groupe... 
		// soit ce test présupose que le groupe et tous ces éléments est chargé dans le BeanMap
		// soit il effectue un test à l'aide du mapper...
		return false;
	}

	public String getItem() {
		return item;
	}

	public String getGroup() {
		return group;
	}

	public String getType() {
		return type;
	}
	
	public void setItem(String item) {
		this.item = item;
	}

	public void setGroup(String group) {
		this.group = group;
	}

	public void setType(String type) {
		this.type = type;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (item != null) {
			try {
				int i = Integer.parseInt(item);
				sb.append(String.format(Messages.Criteria_Item,i));
			} catch (NumberFormatException e) {
				sb.append(item);
				sb.append(' ');
			}
		}
		sb.append(Messages.Criteria_MemberOf);
		try {
			int i = Integer.parseInt(group);
			sb.append(String.format(Messages.Criteria_GroupType, type, i));
		} catch (NumberFormatException e) {
			sb.append(String.format(Messages.Criteria_Group, group));
		}
		return sb.toString();
	}

}
