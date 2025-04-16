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
package com.arcadsoftware.metadata.xml;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.arcadsoftware.metadata.criteria.AbstractLinkTestCriteria;
import com.arcadsoftware.metadata.criteria.AfterCriteria;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.AttributeEqualsCriteria;
import com.arcadsoftware.metadata.criteria.AttributeLowerCriteria;
import com.arcadsoftware.metadata.criteria.AttributeLowerOrEqualsCriteria;
import com.arcadsoftware.metadata.criteria.BeforeCriteria;
import com.arcadsoftware.metadata.criteria.BetweenCriteria;
import com.arcadsoftware.metadata.criteria.ChangedByCriteria;
import com.arcadsoftware.metadata.criteria.ChangedCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.ContainCriteria;
import com.arcadsoftware.metadata.criteria.CurrentUserCriteria;
import com.arcadsoftware.metadata.criteria.DeletedCriteria;
import com.arcadsoftware.metadata.criteria.EndCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.EqualICCriteria;
import com.arcadsoftware.metadata.criteria.GreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.GreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.HasRightCriteria;
import com.arcadsoftware.metadata.criteria.IAttributeCriteria;
import com.arcadsoftware.metadata.criteria.IAttributesCriteria;
import com.arcadsoftware.metadata.criteria.ILinkCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.IsTrueCriteria;
import com.arcadsoftware.metadata.criteria.LinkContainCriteria;
import com.arcadsoftware.metadata.criteria.LinkCriteria;
import com.arcadsoftware.metadata.criteria.LinkEndCriteria;
import com.arcadsoftware.metadata.criteria.LinkEqualCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.LinkGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.LinkStartCriteria;
import com.arcadsoftware.metadata.criteria.LowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LowerThanCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.criteria.StartCriteria;
import com.arcadsoftware.metadata.criteria.SubstCriteria;
import com.arcadsoftware.metadata.criteria.UnlinkCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.osgi.ISODateFormater;

public class JsonCriteriaStream  {

	public JsonCriteriaStream() {
		super();
	}

	/**
	 * Convert an ISearchCriteria into a JSON representation.
	 * 
	 * @param criteria a non null and valid ISearchCriteria.
	 * @return null if an error occurs.
	 */
	public String write(ISearchCriteria criteria) {
		try {
			JSONObject result = convert(criteria);
			return result.toString();
		} catch (JSONException e) {
			if (Activator.getInstance() == null) {
				e.printStackTrace();
			} else {
				Activator.getInstance().error(e);
			}
			return null;
		}
	}
	
	private JSONObject convert(ISearchCriteria criteria) throws JSONException {
		JSONObject result = new JSONObject();
		if (criteria instanceof OrCriteria) {
			result.put("or", convert(((OrCriteria) criteria).getCriterias())); //$NON-NLS-1$
		} else if (criteria instanceof AndCriteria) {
			result.put("and", convert(((AndCriteria) criteria).getCriterias())); //$NON-NLS-1$
		} else if (criteria instanceof NotCriteria) {
			result.put("not", convert(((NotCriteria) criteria).getCriteria())); //$NON-NLS-1$
		} else if (criteria instanceof ConstantCriteria) {
			if (((ConstantCriteria) criteria).isValue()) {
				result.put("constant", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				result.put("constant", "false"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		} else if (criteria instanceof IsNullCriteria) {
			result.put("isnull", ((IsNullCriteria) criteria).getAttribute()); //$NON-NLS-1$
		} else if (criteria instanceof IsTrueCriteria) {
			result.put("istrue", ((IsTrueCriteria) criteria).getAttribute()); //$NON-NLS-1$
		} else if (criteria instanceof InListCriteria) {
			if (((InListCriteria) criteria).getAttribute() == null) {
				result.put("isin", convertSet(((InListCriteria) criteria).getIds())); //$NON-NLS-1$
			} else {
				JSONObject sub = convertAttribute(criteria);
				sub.put("ids", convertSet(((InListCriteria) criteria).getIds())); //$NON-NLS-1$
				result.put("isin", sub); //$NON-NLS-1$
			}
		} else if (criteria instanceof IdGreaterStrictCriteria) {
			result.put("idgreater", ((IdGreaterStrictCriteria) criteria).getId()); //$NON-NLS-1$
		} else if (criteria instanceof IdGreaterThanCriteria) {
			result.put("idgreaterorequals", ((IdGreaterThanCriteria) criteria).getId()); //$NON-NLS-1$
		} else if (criteria instanceof IdLowerStrictCriteria) {
			result.put("idlower", ((IdLowerStrictCriteria) criteria).getId()); //$NON-NLS-1$
		} else if (criteria instanceof IdLowerThanCriteria) {
			result.put("idlowerorequals", ((IdLowerThanCriteria) criteria).getId()); //$NON-NLS-1$
		} else if (criteria instanceof IdEqualCriteria) {
			result.put("idequal", ((IdEqualCriteria) criteria).getId()); //$NON-NLS-1$
		} else if (criteria instanceof EqualCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((EqualCriteria) criteria).getValue() != null) {
				sub.put("value", ((EqualCriteria) criteria).getValue()); //$NON-NLS-1$
			}
			if (((EqualCriteria) criteria).getIntval() != null) {
				sub.put("intval", ((EqualCriteria) criteria).getIntval()); //$NON-NLS-1$
			}
			if (!((EqualCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("equals", sub); //$NON-NLS-1$
		} else if (criteria instanceof LowerThanCriteria) {
			result.put("lowerthan", convertAttribute(criteria).put("value", ((LowerThanCriteria) criteria).getValue())); //$NON-NLS-1$ //$NON-NLS-2$
		} else if (criteria instanceof GreaterThanCriteria) {
			result.put("greaterthan", convertAttribute(criteria).put("value", ((GreaterThanCriteria) criteria).getValue())); //$NON-NLS-1$ //$NON-NLS-2$
		} else if (criteria instanceof LowerStrictCriteria) {
			result.put("lowerstrict", convertAttribute(criteria).put("value", ((LowerStrictCriteria) criteria).getValue())); //$NON-NLS-1$ //$NON-NLS-2$
		} else if (criteria instanceof GreaterStrictCriteria) {
			result.put("greaterstrict", convertAttribute(criteria).put("value", ((GreaterStrictCriteria) criteria).getValue())); //$NON-NLS-1$ //$NON-NLS-2$
		} else if (criteria instanceof ContainCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((ContainCriteria) criteria).getValue()); //$NON-NLS-1$
			if (((ContainCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("contains", sub); //$NON-NLS-1$
		} else if (criteria instanceof StartCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((StartCriteria) criteria).getValue()); //$NON-NLS-1$
			if (((StartCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("starts", sub); //$NON-NLS-1$
		} else if (criteria instanceof EndCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((EndCriteria) criteria).getValue()); //$NON-NLS-1$
			if (((EndCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("ends", sub); //$NON-NLS-1$
		} else if (criteria instanceof LinkCriteria) {
			result.put("linkto", convertAttribute(criteria) //$NON-NLS-1$
					.put("linkCode", ((LinkCriteria) criteria).getLinkCode()) //$NON-NLS-1$
					.put("id", ((LinkCriteria) criteria).getId())); //$NON-NLS-1$
		} else if (criteria instanceof BeforeCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((BeforeCriteria) criteria).getValue() != null) {
				sub.put("value", ISODateFormater.toString(((BeforeCriteria) criteria).getValue())); //$NON-NLS-1$
			}
			if (((BeforeCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (((BeforeCriteria) criteria).getYears() != 0) {
				sub.put("years", ((BeforeCriteria) criteria).getYears()); //$NON-NLS-1$
			}
			if (((BeforeCriteria) criteria).getMonths() != 0) {
				sub.put("months", ((BeforeCriteria) criteria).getMonths()); //$NON-NLS-1$
			}
			if (((BeforeCriteria) criteria).getDays() != 0) {
				sub.put("days", ((BeforeCriteria) criteria).getDays()); //$NON-NLS-1$
			}
			if (((BeforeCriteria) criteria).getHours() != 0) {
				sub.put("hours", ((BeforeCriteria) criteria).getHours()); //$NON-NLS-1$
			}
			if (((BeforeCriteria) criteria).getMinuts() != 0) {
				sub.put("minuts", ((BeforeCriteria) criteria).getMinuts()); //$NON-NLS-1$
			}
			result.put("before", sub); //$NON-NLS-1$
		} else if (criteria instanceof AfterCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((AfterCriteria) criteria).getValue() != null) {
				sub.put("value", ISODateFormater.toString(((AfterCriteria) criteria).getValue())); //$NON-NLS-1$
			}
			if (((AfterCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (((AfterCriteria) criteria).getYears() != 0) {
				sub.put("years", ((AfterCriteria) criteria).getYears()); //$NON-NLS-1$
			}
			if (((AfterCriteria) criteria).getMonths() != 0) {
				sub.put("months", ((AfterCriteria) criteria).getMonths()); //$NON-NLS-1$
			}
			if (((AfterCriteria) criteria).getDays() != 0) {
				sub.put("days", ((AfterCriteria) criteria).getDays()); //$NON-NLS-1$
			}
			if (((AfterCriteria) criteria).getHours() != 0) {
				sub.put("hours", ((AfterCriteria) criteria).getHours()); //$NON-NLS-1$
			}
			if (((AfterCriteria) criteria).getMinuts() != 0) {
				sub.put("minuts", ((AfterCriteria) criteria).getMinuts()); //$NON-NLS-1$
			}
			result.put("after", sub); //$NON-NLS-1$
		} else if (criteria instanceof BetweenCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((BetweenCriteria) criteria).getBefore() != null) {
				sub.put("before", ISODateFormater.toString(((BetweenCriteria) criteria).getBefore())); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getAfter() != null) {
				sub.put("after", ISODateFormater.toString(((BetweenCriteria) criteria).getAfter())); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (((BetweenCriteria) criteria).getAfteryears() != 0) {
				sub.put("afteryears", ((BetweenCriteria) criteria).getAfteryears()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getAftermonths() != 0) {
				sub.put("aftermonths", ((BetweenCriteria) criteria).getAftermonths()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getAfterdays() != 0) {
				sub.put("afterdays", ((BetweenCriteria) criteria).getAfterdays()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getAfterhours() != 0) {
				sub.put("afterhours", ((BetweenCriteria) criteria).getAfterhours()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getAfterminuts() != 0) {
				sub.put("afterminuts", ((BetweenCriteria) criteria).getAfterminuts()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getBeforeyears() != 0) {
				sub.put("beforeyears", ((BetweenCriteria) criteria).getBeforeyears()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getBeforemonths() != 0) {
				sub.put("beforemonths", ((BetweenCriteria) criteria).getBeforemonths()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getBeforedays() != 0) {
				sub.put("beforedays", ((BetweenCriteria) criteria).getBeforedays()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getBeforehours() != 0) {
				sub.put("beforehours", ((BetweenCriteria) criteria).getBeforehours()); //$NON-NLS-1$
			}
			if (((BetweenCriteria) criteria).getBeforeminuts() != 0) {
				sub.put("beforeminuts", ((BetweenCriteria) criteria).getBeforeminuts()); //$NON-NLS-1$
			}
			result.put("between", sub); //$NON-NLS-1$
		} else if (criteria instanceof EqualICCriteria) {
			result.put("equalsic", convertAttribute(criteria).put("value", ((EqualICCriteria) criteria).getValue())); //$NON-NLS-1$ //$NON-NLS-2$
		} else if (criteria instanceof HasRightCriteria) {
			JSONObject sub = convertAttribute(criteria).put("right", ((HasRightCriteria) criteria).getRight()); //$NON-NLS-1$
			if (((HasRightCriteria) criteria).getParam() != null) {
				sub.put("param", ((HasRightCriteria) criteria).getParam()); //$NON-NLS-1$
			}
			result.put("hasRight", sub); //$NON-NLS-1$
		} else if (criteria instanceof CurrentUserCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((CurrentUserCriteria) criteria).getLinkCode() != null) {
				sub.put("linkcode", ((CurrentUserCriteria) criteria).getLinkCode()); //$NON-NLS-1$
			}
			if (((CurrentUserCriteria) criteria).getUserAttribute() != null) {
				sub.put("userAttribute", ((CurrentUserCriteria) criteria).getUserAttribute()); //$NON-NLS-1$
			}
			result.put("currentUser", sub); //$NON-NLS-1$
		} else if (criteria instanceof SubstCriteria) {
			result.put("subst", ((SubstCriteria) criteria).getCode()); //$NON-NLS-1$
		} else if (criteria instanceof AttributeEqualsCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (!((AttributeEqualsCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("equalsAttributes", sub); //$NON-NLS-1$
		} else if (criteria instanceof AttributeLowerCriteria) {
			result.put("lowerAttributes", convertAttribute(criteria)); //$NON-NLS-1$
		} else if (criteria instanceof AttributeLowerOrEqualsCriteria) {
			result.put("lowerEqualsAttributes", convertAttribute(criteria)); //$NON-NLS-1$
		} else if (criteria instanceof ChangedCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((ChangedCriteria) criteria).getBefore() != null) {
				sub.put("before", ISODateFormater.toString(((ChangedCriteria) criteria).getBefore())); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getAfter() != null) {
				sub.put("after", ISODateFormater.toString(((ChangedCriteria) criteria).getAfter())); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (((ChangedCriteria) criteria).getAfteryears() != 0) {
				sub.put("afteryears", ((ChangedCriteria) criteria).getAfteryears()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getAftermonths() != 0) {
				sub.put("aftermonths", ((ChangedCriteria) criteria).getAftermonths()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getAfterdays() != 0) {
				sub.put("afterdays", ((ChangedCriteria) criteria).getAfterdays()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getAfterhours() != 0) {
				sub.put("afterhours", ((ChangedCriteria) criteria).getAfterhours()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getAfterminuts() != 0) {
				sub.put("afterminuts", ((ChangedCriteria) criteria).getAfterminuts()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getBeforeyears() != 0) {
				sub.put("beforeyears", ((ChangedCriteria) criteria).getBeforeyears()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getBeforemonths() != 0) {
				sub.put("beforemonths", ((ChangedCriteria) criteria).getBeforemonths()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getBeforedays() != 0) {
				sub.put("beforedays", ((ChangedCriteria) criteria).getBeforedays()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getBeforehours() != 0) {
				sub.put("beforehours", ((ChangedCriteria) criteria).getBeforehours()); //$NON-NLS-1$
			}
			if (((ChangedCriteria) criteria).getBeforeminuts() != 0) {
				sub.put("beforeminuts", ((ChangedCriteria) criteria).getBeforeminuts()); //$NON-NLS-1$
			}
			result.put("changed", sub); //$NON-NLS-1$
		} else if (criteria instanceof ChangedByCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((ChangedByCriteria) criteria).getUid() != 0) {
				sub.put("uid", ((ChangedByCriteria) criteria).getUid()); //$NON-NLS-1$
			}
			result.put("changedby", sub); //$NON-NLS-1$
		} else if (criteria instanceof DeletedCriteria) {
			result.put("deleted", ((DeletedCriteria) criteria).getAttribute()); //$NON-NLS-1$
		} else if (criteria instanceof UnlinkCriteria) {
			result.put("unlinkto", convertAttribute(criteria) //$NON-NLS-1$
					.put("linkCode", ((UnlinkCriteria) criteria).getLinkCode()) //$NON-NLS-1$
					.put("id", ((UnlinkCriteria) criteria).getId())); //$NON-NLS-1$
		} else if (criteria instanceof LinkEqualCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (!((EqualCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("linkequals", sub); //$NON-NLS-1$
		} else if (criteria instanceof LinkStartCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkStartCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("linkStarts", sub); //$NON-NLS-1$
		} else if (criteria instanceof LinkEndCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkEndCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("linkEnds", sub); //$NON-NLS-1$
		} else if (criteria instanceof LinkContainCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkContainCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			result.put("linkContains", sub); //$NON-NLS-1$
		} else if (criteria instanceof LinkGreaterStrictCriteria) {
			result.put("linkgreaterstrict", convertAttribute(criteria)); //$NON-NLS-1$
		} else if (criteria instanceof LinkGreaterThanCriteria) {
			result.put("linkgreaterthan", convertAttribute(criteria)); //$NON-NLS-1$
		} else {
			throw new JSONException("Unexpected Criteria, unable to convert it in JSON:" + criteria);
		}
		return result;
	}

	private JSONObject convertAttribute(ISearchCriteria criteria) throws JSONException {
		JSONObject result = new JSONObject();
		if ((criteria instanceof IAttributeCriteria) && (((IAttributeCriteria) criteria).getAttribute() != null)) {
			result.put("attribute", ((IAttributeCriteria) criteria).getAttribute()); //$NON-NLS-1$
		}
		if ((criteria instanceof IAttributesCriteria) && (((IAttributesCriteria) criteria).getSecondAttribute() != null)) {
			result.put("secondAttribute", ((IAttributesCriteria) criteria).getSecondAttribute()); //$NON-NLS-1$
		}
		if (criteria instanceof AbstractLinkTestCriteria) {
			result.put("link", ((AbstractLinkTestCriteria) criteria).getLinkCode()); //$NON-NLS-1$
			result.put("value", ((AbstractLinkTestCriteria) criteria).getValue()); //$NON-NLS-1$
			if (((AbstractLinkTestCriteria) criteria).getReference() != null) {
				result.put("reference", ((AbstractLinkTestCriteria) criteria).getReference()); //$NON-NLS-1$
			}
		}
		if (criteria instanceof ILinkCriteria) {
			if (((ILinkCriteria) criteria).isIgnoreSubdivision()) {
				result.put("ignoresd", true); //$NON-NLS-1$
			}
			if (((ILinkCriteria) criteria).isDeletedLinks()) {
				result.put("deleted", true); //$NON-NLS-1$
			}
		}
		return result;
	}

	private JSONArray convertSet(Set<Integer> integers) {
		JSONArray result = new JSONArray();
		for (Integer i: integers) {
			result.put(i);
		}
		return result;
	}

	private JSONArray convert(ArrayList<ISearchCriteria> criterias) throws JSONException {
		JSONArray result = new JSONArray();
		for (ISearchCriteria criteria: criterias) {
			result.put(convert(criteria));
		}
		return result;
	}

	/**
	 * Convert a JSON string in a ISearchCriteria
	 * 
	 * @param json a non null and valid JSON string.
	 * @return null is a problem occurs.
	 */
	public ISearchCriteria read(String json) {
		try {
			return convert(new JSONObject(json));
		} catch (Exception e) {
			if (Activator.getInstance() == null) {
				e.printStackTrace();
			} else {
				Activator.getInstance().error(e);
			}
			return null;
		}
	}

	private ISearchCriteria convert(JSONObject o) throws JSONException {
		if (o.length() == 0) {
			return null;
		}
		if (o.length() == 1) {
			Iterator<?> itt = o.keys();
			String key = itt.next().toString();
			return convert(key, o.get(key));
		}
		OrCriteria result = new OrCriteria();
		Iterator<?> itt = o.keys();
		while (itt.hasNext()) {
			String key = itt.next().toString();
			result.add(convert(key, o.get(key)));
		}
		return result;
	}
	
	private ISearchCriteria convert(String name, Object value) throws JSONException {
		switch (name.toLowerCase()) {
		case "or": //$NON-NLS-1$
			return new OrCriteria(convert(((JSONArray) value)));
		case "and": //$NON-NLS-1$
			return new AndCriteria(convert(((JSONArray) value)));
		case "not": //$NON-NLS-1$
			return new NotCriteria(convert(((JSONObject) value)));
		case "constant": //$NON-NLS-1$
			return new ConstantCriteria((value != null) && //
					("true".equalsIgnoreCase(value.toString()) ||  //$NON-NLS-1$
							"yes".equalsIgnoreCase(value.toString()) || //$NON-NLS-1$ 
							"1".equals(value.toString()))); //$NON-NLS-1$
		case "isnull": //$NON-NLS-1$
		case "isnul": //$NON-NLS-1$
		case "null": //$NON-NLS-1$
		case "nul": //$NON-NLS-1$
			return new IsNullCriteria((String) value);
		case "idequals": //$NON-NLS-1$
		case "idequal": //$NON-NLS-1$
		case "ideq": //$NON-NLS-1$
		case "id": //$NON-NLS-1$
			return new IdEqualCriteria((String) value);
		case "istrue": //$NON-NLS-1$
			return new IsTrueCriteria((String) value);
		case "subst": //$NON-NLS-1$
		case "sub": //$NON-NLS-1$
			return new SubstCriteria((String) value);
		case "deleted": //$NON-NLS-1$
		case "del": //$NON-NLS-1$
			return new DeletedCriteria((String) value);
		case "idgreaterstrict": //$NON-NLS-1$
		case "idgreater": //$NON-NLS-1$
		case "idgs": //$NON-NLS-1$
			return new IdGreaterStrictCriteria((String) value);
		case "idgreaterorequals": //$NON-NLS-1$
		case "idgreaterthan": //$NON-NLS-1$
		case "idgt": //$NON-NLS-1$
			return new IdGreaterThanCriteria((String) value);
		case "idlowerstrict": //$NON-NLS-1$
		case "idlower": //$NON-NLS-1$
		case "idls": //$NON-NLS-1$
			return new IdLowerStrictCriteria((String) value);
		case "idlowerorequals": //$NON-NLS-1$
		case "idlowerthan": //$NON-NLS-1$
		case "idlt": //$NON-NLS-1$
			return new IdLowerThanCriteria((String) value);
		case "isin": //$NON-NLS-1$
		case "idin": //$NON-NLS-1$
		case "in": //$NON-NLS-1$
			InListCriteria result = new InListCriteria();
			JSONArray ids = null;
			if (value instanceof JSONObject) {
				fill(result, (JSONObject) value);
				try {
					ids = ((JSONObject) value).getJSONArray("ids");
				} catch (JSONException e) {}
			} else if (value instanceof JSONArray) {
				ids = (JSONArray) value;
			}
			if (ids != null) {
				int l = ((JSONArray) value).length();
				for (int i = 0; i < l; i++) {
					Object o = ((JSONArray) value).get(i);
					if (o instanceof Integer) {
						result.add((Integer) o);
					} else if (o instanceof String) {
						try {
							result.add(Integer.parseInt((String) o));
						} catch (NumberFormatException e) {
							// ignore this element.
						}
					}
				}
			}
			return result;
		case "equals": //$NON-NLS-1$
		case "equal": //$NON-NLS-1$
		case "eq": //$NON-NLS-1$
			return fill(new EqualCriteria(), (JSONObject) value);
		case "linkgreaterstrict": //$NON-NLS-1$
		case "linkgreater": //$NON-NLS-1$
		case "lgreaterstrict": //$NON-NLS-1$
		case "lgreater": //$NON-NLS-1$
		case "lgs": //$NON-NLS-1$
			return fill(new LinkGreaterStrictCriteria(), (JSONObject) value);
		case "linkgreaterthan": //$NON-NLS-1$
		case "lgreaterthan": //$NON-NLS-1$
		case "lgt": //$NON-NLS-1$
			return fill(new LinkGreaterThanCriteria(), (JSONObject) value);
		case "lowerthan": //$NON-NLS-1$
		case "lt": //$NON-NLS-1$
			return fill(new LowerThanCriteria(), (JSONObject) value);
		case "greaterthan": //$NON-NLS-1$
		case "gt": //$NON-NLS-1$
			return fill(new GreaterThanCriteria(), (JSONObject) value);
		case "lowerstrict": //$NON-NLS-1$
		case "lower": //$NON-NLS-1$
		case "ls": //$NON-NLS-1$
			return fill(new LowerStrictCriteria(), (JSONObject) value);
		case "greaterstrict": //$NON-NLS-1$
		case "greater": //$NON-NLS-1$
		case "gs": //$NON-NLS-1$
			return fill(new GreaterStrictCriteria(), (JSONObject) value);
		case "contains": //$NON-NLS-1$
		case "contain": //$NON-NLS-1$
			return fill(new ContainCriteria(), (JSONObject) value);
		case "starts": //$NON-NLS-1$
			return fill(new StartCriteria(), (JSONObject) value);
		case "ends": //$NON-NLS-1$
			return fill(new EndCriteria(), (JSONObject) value);
		case "linkto": //$NON-NLS-1$
		case "link": //$NON-NLS-1$
		case "lto": //$NON-NLS-1$
			return fill(new LinkCriteria(), (JSONObject) value);
		case "before": //$NON-NLS-1$
			return fill(new BeforeCriteria(), (JSONObject) value);
		case "after": //$NON-NLS-1$
			return fill(new AfterCriteria(), (JSONObject) value);
		case "between": //$NON-NLS-1$
			return fill(new BetweenCriteria(), (JSONObject) value);
		case "linkequals": //$NON-NLS-1$
		case "linkequal": //$NON-NLS-1$
		case "lequals": //$NON-NLS-1$
		case "lequal": //$NON-NLS-1$
		case "leq": //$NON-NLS-1$
			return fill(new LinkEqualCriteria(), (JSONObject) value);
		case "equalsic": //$NON-NLS-1$
		case "eqic": //$NON-NLS-1$
			EqualCriteria c = fill(new EqualCriteria(), (JSONObject) value);
			c.setCasesensitive(false);
			return c;
		case "hasright": //$NON-NLS-1$
		case "right": //$NON-NLS-1$
			return fill(new HasRightCriteria(), (JSONObject) value);
		case "currentuser": //$NON-NLS-1$
		case "user": //$NON-NLS-1$
			return fill(new CurrentUserCriteria(), (JSONObject) value);
		case "equalsattributes": //$NON-NLS-1$
		case "equalsa": //$NON-NLS-1$
		case "eqa": //$NON-NLS-1$
			return fill(new AttributeEqualsCriteria(), (JSONObject) value);
		case "lowerstrictattributes": //$NON-NLS-1$
		case "lowerattributes": //$NON-NLS-1$
		case "lsa": //$NON-NLS-1$
			return fill(new AttributeLowerCriteria(), (JSONObject) value);
		case "lowerthanattributes": //$NON-NLS-1$
		case "lowerequalsattributes": //$NON-NLS-1$
		case "lowerequalsa": //$NON-NLS-1$
		case "lowerthana": //$NON-NLS-1$
		case "lowerequala": //$NON-NLS-1$
		case "lta": //$NON-NLS-1$
			return fill(new AttributeLowerOrEqualsCriteria(), (JSONObject) value);
		case "changed": //$NON-NLS-1$
			return fill(new ChangedCriteria(), (JSONObject) value);
		case "changedby": //$NON-NLS-1$
			return fill(new ChangedByCriteria(), (JSONObject) value);
		case "unlinkto": //$NON-NLS-1$
		case "unlink": //$NON-NLS-1$
		case "unl": //$NON-NLS-1$
			return fill(new UnlinkCriteria(), (JSONObject) value);
		case "linkstarts": //$NON-NLS-1$
		case "linkstart": //$NON-NLS-1$
		case "lstarts": //$NON-NLS-1$
		case "lstart": //$NON-NLS-1$
			return fill(new LinkStartCriteria(), (JSONObject) value);
		case "linkends": //$NON-NLS-1$
		case "linkend": //$NON-NLS-1$
		case "lends": //$NON-NLS-1$
		case "lend": //$NON-NLS-1$
			return fill(new LinkEndCriteria(), (JSONObject) value);
		case "linkcontains": //$NON-NLS-1$
		case "linkcontain": //$NON-NLS-1$
		case "lcontains": //$NON-NLS-1$
		case "lcontain": //$NON-NLS-1$
			return fill(new LinkContainCriteria(), (JSONObject) value);
		}
		return null;
	}

	private <I extends ISearchCriteria> I fill(I criteria, JSONObject value) throws JSONException {
		if (value != null) {
			Iterator<?> itt = value.keys();
			while (itt.hasNext()) {
				String key = itt.next().toString();
				Object o = value.get(key);
				if (o != null) {
					switch(key.toLowerCase()) {
					case "attribute": //$NON-NLS-1$
					case "att": //$NON-NLS-1$
					case "a": //$NON-NLS-1$
						if (criteria instanceof IAttributeCriteria) {
							((IAttributeCriteria) criteria).setAttribute(o.toString());
						}
						break;
					case "secondattribute": //$NON-NLS-1$
					case "secatt": //$NON-NLS-1$
					case "sa": //$NON-NLS-1$
						if (criteria instanceof CurrentUserCriteria) {
							((CurrentUserCriteria) criteria).setUserAttribute(o.toString());
						}
						break;
					case "userattribute": //$NON-NLS-1$
					case "useratt": //$NON-NLS-1$
					case "uatt": //$NON-NLS-1$
					case "ua": //$NON-NLS-1$
						if (criteria instanceof IAttributeCriteria) {
							((IAttributeCriteria) criteria).setAttribute(o.toString());
						}
						break;
					case "linkcode": //$NON-NLS-1$
					case "link": //$NON-NLS-1$
					case "lc": //$NON-NLS-1$
					case "l": //$NON-NLS-1$
						if (criteria instanceof ILinkCriteria) {
							((ILinkCriteria) criteria).setLinkCode(o.toString());
						} else if (criteria instanceof CurrentUserCriteria) {
							((CurrentUserCriteria) criteria).setLinkCode(o.toString());
						}
						break;
					case "reference": //$NON-NLS-1$
					case "ref": //$NON-NLS-1$
					case "r": //$NON-NLS-1$
						if (criteria instanceof AbstractLinkTestCriteria) {
							((AbstractLinkTestCriteria) criteria).setReference(o.toString());
						}
						break;
					case "value": //$NON-NLS-1$
					case "val": //$NON-NLS-1$
					case "v": //$NON-NLS-1$
						if (criteria instanceof AbstractLinkTestCriteria) {
							((AbstractLinkTestCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof EqualCriteria) {
							((EqualCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof LowerThanCriteria) {
							((LowerThanCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof GreaterThanCriteria) {
							((GreaterThanCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof LowerStrictCriteria) {
							((LowerStrictCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof GreaterStrictCriteria) {
							((GreaterStrictCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof ContainCriteria) {
							((ContainCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof StartCriteria) {
							((StartCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof EndCriteria) {
							((EndCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setValue(getDate(o));
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setValue(getDate(o));
						}
						break;
					case "casesensitive": //$NON-NLS-1$
					case "case": //$NON-NLS-1$
					case "cs": //$NON-NLS-1$
						boolean b = getBoolean(o);
						if (criteria instanceof EqualCriteria) {
							((EqualCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof ContainCriteria) {
							((ContainCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof StartCriteria) {
							((StartCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof EndCriteria) {
							((EndCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof LinkEqualCriteria) {
							((LinkEqualCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof LinkStartCriteria) {
							((LinkStartCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof LinkEndCriteria) {
							((LinkEndCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof LinkContainCriteria) {
							((LinkContainCriteria) criteria).setCasesensitive(b);
						} else if (criteria instanceof AttributeEqualsCriteria) {
							((AttributeEqualsCriteria) criteria).setCasesensitive(b);
						}
						break;
					case "intval": //$NON-NLS-1$
					case "iv": //$NON-NLS-1$
						int i = getInteger(o);
						if (criteria instanceof EqualCriteria) {
							((EqualCriteria) criteria).setIntval(i);
						}
						break;
					case "id": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof LinkCriteria) {
							((LinkCriteria) criteria).setId(i);
						} else if (criteria instanceof UnlinkCriteria) {
							((UnlinkCriteria) criteria).setId(i);
						}
						break;
					case "trunc": //$NON-NLS-1$
					case "tc": //$NON-NLS-1$
					case "t": //$NON-NLS-1$
						b = getBoolean(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setTrunc(b);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setTrunc(b);
						} else if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setTrunc(b);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setTrunc(b);
						}
						break;
					case "param": //$NON-NLS-1$
					case "p": //$NON-NLS-1$
						if (criteria instanceof HasRightCriteria) {
							((HasRightCriteria) criteria).setParam(o.toString());
						}
						break;
					case "right": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof HasRightCriteria) {
							((HasRightCriteria) criteria).setRight(i);
						}
						break;
					case "before": //$NON-NLS-1$
						Date d = getDate(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBefore(d);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBefore(d);
						}
						break;
					case "after": //$NON-NLS-1$
						d = getDate(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfter(d);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfter(d);
						}
						break;
					case "years": //$NON-NLS-1$
					case "yr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setYears(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setYears(i);
						}
						break;
					case "months": //$NON-NLS-1$
					case "mt": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setMonths(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setMonths(i);
						}
						break;
					case "days": //$NON-NLS-1$
					case "dy": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setDays(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setDays(i);
						}
						break;
					case "hours": //$NON-NLS-1$
					case "hr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setHours(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setHours(i);
						}
						break;
					case "minuts": //$NON-NLS-1$
					case "mn": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setMinuts(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setMinuts(i);
						}
						break;
					case "includelimits": //$NON-NLS-1$
					case "limits": //$NON-NLS-1$
						b = getBoolean(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setIncludeLimits(b);
						}
						break;
					case "afteryears": //$NON-NLS-1$
					case "ayr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfteryears(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfteryears(i);
						}
						break;
					case "aftermonths": //$NON-NLS-1$
					case "amt": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAftermonths(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAftermonths(i);
						}
						break;
					case "afterdays": //$NON-NLS-1$
					case "ady": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterdays(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterdays(i);
						}
						break;
					case "afterhours": //$NON-NLS-1$
					case "ahr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterhours(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterhours(i);
						}
						break;
					case "afterminuts": //$NON-NLS-1$
					case "amn": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterminuts(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterminuts(i);
						}
						break;
					case "beforeyears": //$NON-NLS-1$
					case "byr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforeyears(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforeyears(i);
						}
						break;
					case "beforemonths": //$NON-NLS-1$
					case "bmt": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforemonths(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforemonths(i);
						}
						break;
					case "beforedays": //$NON-NLS-1$
					case "bdy": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforedays(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforedays(i);
						}
						break;
					case "beforehours": //$NON-NLS-1$
					case "bhr": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforehours(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforehours(i);
						}
						break;
					case "beforeminuts": //$NON-NLS-1$
					case "bmn": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforeminuts(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforeminuts(i);
						}
						break;
					case "ignoresubdivision": //$NON-NLS-1$
					case "ignoresd": //$NON-NLS-1$
					case "nosd": //$NON-NLS-1$
						b = getBoolean(o);
						if (criteria instanceof ILinkCriteria) {
							((ILinkCriteria) criteria).setIgnoreSubdivision(b);
						}
						break;
					case "deletedlinks": //$NON-NLS-1$
					case "deleted": //$NON-NLS-1$
					case "del": //$NON-NLS-1$
						b = getBoolean(o);
						if (criteria instanceof ILinkCriteria) {
							((ILinkCriteria) criteria).setDeletedLinks(b);
						}
						break;
					case "uid": //$NON-NLS-1$
						i = getInteger(o);
						if (criteria instanceof ChangedByCriteria) {
							((ChangedByCriteria) criteria).setUid(i);
						}
						break;
					}
				}
			}
		}
		return criteria;
	}

	private Date getDate(Object o) {
		if (o instanceof Date) {
			return (Date) o;
		}
		if (o instanceof Calendar) {
			return ((Calendar) o).getTime();
		}
		try {
			return ISODateFormater.toDate(o.toString());
		} catch (ParseException e) {
			return null;
		}
	}

	private int getInteger(Object o) {
		if (o instanceof Integer) {
			return ((Integer) o).intValue();
		}
		try {
			return Integer.parseInt(o.toString().trim());
		} catch (NumberFormatException e) {}
		return 0;
	}

	private boolean getBoolean(Object o) {
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		if (o instanceof Integer) {
			return ((Integer) o).intValue() != 0;
		}
		String s = o.toString();
		return "true".equalsIgnoreCase(s) || //$NON-NLS-1$
				"yes".equalsIgnoreCase(s) || //$NON-NLS-1$
				"1".equals(s); //$NON-NLS-1$
	}
	
	private ArrayList<ISearchCriteria> convert(JSONArray a) throws JSONException {
		int l = a.length();
		ArrayList<ISearchCriteria> result = new ArrayList<ISearchCriteria>(l);
		for (int i = 0; i < l; i++) { 
			JSONObject o = a.getJSONObject(i);
			if ((o != null) && (o.length() > 0)) {
				Iterator<?> itt = o.keys();
				while (itt.hasNext()) {
					String key = itt.next().toString();
					result.add(convert(key, o.get(key)));
				}
			}
		}
		return result;
	}
	
}