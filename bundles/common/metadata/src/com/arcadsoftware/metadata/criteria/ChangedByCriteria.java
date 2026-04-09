package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * Check if the given user (or the current user) is the last user to 
 * have modified the selected data, or the referenced one through "attribute".
 * 
 * <p>
 * The formal test use the "muidCol" data of the entity. 
 * 
 * <p>
 * If "attribute" is null or empty, the current selected entity is tested.
 * 
 * <p>
 * If "uid" is set to zero the current user is used for the test. If no current user is avaible the test fail.
 * 
 * <p>
 * If the entity is read-only, not "updatable" or do not define a "muidCol" metadata, then the test fail.
 * 
 * @author ARCAD Software
 */
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
		// Read only data can not be changed, non updatables or without updateCol neither.
		if (e.isReadOnly() || //
				(!e.getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) && // 
				(e.getMetadata().get("muidCol") == null))) { //$NON-NLS-1$
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
