package com.arcadsoftware.metadata.criteria;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

public class InSubdivisionCriteria extends AbstractSearchCriteria implements Cloneable, IAttributeCriteria {

	private String attribute;
	private Integer value;

	public InSubdivisionCriteria() {
		super();
	}

	public InSubdivisionCriteria(String attribute, int value) {
		super();
		this.attribute = attribute;
		this.value = value;
	}
	
	@Override
	public ISearchCriteria reduce(ICriteriaContext context) {
		ReferenceLine refline = context.getEntity().getReferenceLine(attribute);
		if (refline == null) {
			return ConstantCriteria.FALSE;
		}
		if (refline.isMultiLinkList()) {
			Activator.getInstance().warn("In Subdivision Criteria with multi-link references is not supported: " + toString());
			return ConstantCriteria.FALSE;
		}
		/* TODO Add a criteria "InLinkSubdivisionCriteria"...
		if (refline.isLinkList()) {
			String preLinkCode = refline.getPreLinkCodes();
			if (preLinkCode.isEmpty()) {
				preLinkCode = null;
			}
			String postLinkCode = refline.getPostLinkCodes();
			if (postLinkCode.isEmpty()) {
				int i = 0;
				if (intval != null) {
					i = intval;
				} else if (value != null) {
					try {
						i = Integer.parseInt(value);
					} catch (NumberFormatException e) {
						Activator.getInstance().warn("Invalid Equals Criteria with terminal link reference: " + toString());
					}
				}
				return new LinkCriteria(i, refline.getFirstLinkCode(), preLinkCode).reduce(context);
			}
			LinkEqualCriteria lec;
			if (intval != null) {
				lec = new LinkEqualCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode, null, intval.toString(), (casesensitive == null) || casesensitive);
			} else {
				lec = new LinkEqualCriteria(preLinkCode, refline.getFirstLinkCode(), postLinkCode, null, value, (casesensitive == null) || casesensitive);
			}
			return lec.reduce(context);
		} */
		context.useReference(refline);
		MetaDataEntity ref_entity = refline.getLastAttribute().getRefEntity();
		if ((ref_entity == null) || !ref_entity.hasRecursiveLink()) {
			return new EqualCriteria(attribute, value);
		}
		return this;
	}

	@Override
	public boolean test(BeanMap bean, IConnectionUserBean currentUser) {
		// the "sub-set of BeanMap require an external information, we can only test the current attribute value...
		return value == bean.getInt(attribute);
	}

	@Override
	public Object clone() {
		return new InSubdivisionCriteria(attribute, value);
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof InSubdivisionCriteria) &&
			nullsOrEquals(attribute, ((InSubdivisionCriteria) obj).attribute) &&
			(value == ((InSubdivisionCriteria) obj).value);
	}

	@Override
	public String getAttribute() {
		return attribute;
	}

	public int getValue() {
		return value;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (attribute != null) {
			sb.append(attribute);
		}
		sb.append(Messages.Criteria_InSet);
		sb.append(Messages.Criteria_Recursive);
		sb.append(value);
		return sb.toString();
	}

	@Override
	public void setAttribute(String code) {
		attribute = code;
	}
	
	public void setValue(int value) {
		this.value = value;
	}
}
