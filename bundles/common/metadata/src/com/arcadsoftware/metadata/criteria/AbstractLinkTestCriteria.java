package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public abstract class AbstractLinkTestCriteria extends AbstractSearchCriteria implements IAttributeCriteria {

	private String linkCode;
	private String reference;
	private String attribute;
	private String value;

	public AbstractLinkTestCriteria(String linkCode, String attribute, String value) {
		super();
		this.linkCode = linkCode;
		this.attribute = attribute;
		this.value = value;
	}

	public AbstractLinkTestCriteria(String reference, String linkCode, String attribute, String value) {
		this(linkCode, attribute, value);
		this.reference = reference;
	}
	
	public AbstractLinkTestCriteria() {
		super();
	}

	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		MetaDataEntity entity;
		ReferenceLine referenceRef = null;
		if (reference == null) {
			entity = context.getEntity();
		} else {
			referenceRef = context.getEntity().getAttributeLine(reference);
			if ((referenceRef != null) && (referenceRef.size() > 0)) {
				entity = referenceRef.getLastAttribute().getRefEntity();
			} else {
				return ConstantCriteria.FALSE;
			}
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link != null) {
			MetaDataEntity e = link.getRefEntity();
			if (e != null) {
				ReferenceLine ref = e.getAttributeLine(attribute);
				if (ref != null) {
					context.useLinkReference(link, ref);
					if (referenceRef != null) {
						context.useReference(referenceRef);
					}
					return this;
				}
			}
		}
		return ConstantCriteria.FALSE;
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof AbstractLinkTestCriteria) && // 
				nullsOrEquals(value, ((AbstractLinkTestCriteria) obj).value) && //
				nullsOrEquals(attribute, ((AbstractLinkTestCriteria) obj).attribute) && //
				nullsOrEquals(reference, ((AbstractLinkTestCriteria) obj).reference) && //
				nullsOrEquals(linkCode, ((AbstractLinkTestCriteria) obj).linkCode);
	}
	
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		if ((value == null) || (value.length() == 0)) {
			return false;
		}
		if (reference != null) {
			Object o = bean.get(reference);
			if (o instanceof BeanMap) {
				bean = (BeanMap) o;
			} else {
				return false;
			}
		}
		Object o = bean.get(linkCode);
		if (o instanceof BeanMapList) {
			for (BeanMap b: (BeanMapList) o) {
				if (test(b.getString(attribute), value)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Perform the criteria test on these two values.
	 * 
	 * @param attributeValue The value coming from the attribute reference.
	 * @param value The constant value (this value is not null).
	 * @return
	 */
	protected abstract boolean test(Object attributeValue, Object value);

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (reference != null) {
			sb.append(reference);
			sb.append(' ');
		}
		sb.append(String.format(Messages.Criteria_LinkedThrough, linkCode));
		sb.append(attribute);
		sb.append(getTestString());
		sb.append('"');
		sb.append(value);
		sb.append('"');
		return sb.toString();
	}
	
	/**
	 * Get a textual representation of this operator.
	 * @return
	 */
	protected abstract String getTestString();

	public String getLinkCode() {
		return linkCode;
	}

	public void setLinkCode(String linkCode) {
		this.linkCode = linkCode;
	}

	public String getReference() {
		return reference;
	}

	public void setReference(String reference) {
		this.reference = reference;
	}

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

}
