package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class ChangedByCriteria extends AbstractSearchCriteria implements IAttributeCriteria {

	private String attribute;
	private int uid;

	public ChangedByCriteria() {
		super();
	}

	public ChangedByCriteria(int uid) {
		super();
		this.uid = uid;
	}

	public ChangedByCriteria(String attribute, int uid) {
		super();
		this.attribute = attribute;
		this.uid = uid;
	}

	@Override
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) { 
		if (uid == 0) {
			return (currentUser != null) && (bean.getMUID() == currentUser.getId());
		}
		return false;
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		MetaDataEntity e = context.getEntity();
		ReferenceLine rl = null;
		if ((attribute != null) && !attribute.isEmpty()) {
			rl = e.getAttributeLine(attribute);
			if ((rl == null) || (rl.getLastEntity() == null)) {
				return ConstantCriteria.FALSE;
			}
			e = rl.getLastEntity();
		}
		if (e.isReadOnly() || //
				!e.getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) || // 
				(e.getMetadata().get("muidCol") == null)) { //$NON-NLS-1$
			return ConstantCriteria.FALSE;
		}
		if (rl != null) {
			context.useReference(rl);
		}
		return this; // unchanged object...
	}

	@Override
	public ChangedByCriteria clone() {
		return new ChangedByCriteria(attribute, uid);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof ChangedByCriteria) &&
				(((ChangedByCriteria) obj).uid == uid) &&
				nullsOrEquals(((ChangedByCriteria) obj).attribute, attribute);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if ((attribute != null) && !attribute.isEmpty()) {
			sb.append(attribute);
			sb.append(" has "); //$NON-NLS-1$
		}
		sb.append("changed by "); //$NON-NLS-1$
		if (uid == 0) {
			sb.append("current"); //$NON-NLS-1$
		} else {
			sb.append('#');
			sb.append(uid);
		}
		sb.append(" user"); //$NON-NLS-1$
		return sb.toString();
	}

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public int getUid() {
		return uid;
	}

	public void setUid(int uid) {
		this.uid = uid;
	}

}
