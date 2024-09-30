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
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.InListCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.InGroupCriteria;
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
import com.arcadsoftware.metadata.criteria.PreGeneratedCriteria;
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
			result.put("or", convert(((OrCriteria) criteria).getCriterias()));
		} else if (criteria instanceof AndCriteria) {
			result.put("and", convert(((AndCriteria) criteria).getCriterias()));
		} else if (criteria instanceof NotCriteria) {
			result.put("not", convert(((NotCriteria) criteria).getCriteria()));
		} else if (criteria instanceof ConstantCriteria) {
			if (((ConstantCriteria) criteria).isValue()) {
				result.put("constant", "true");
			} else {
				result.put("constant", "false");
			}
		} else if (criteria instanceof IsNullCriteria) {
			result.put("isnull", ((IsNullCriteria) criteria).getAttribute());
		} else if (criteria instanceof IsTrueCriteria) {
			result.put("istrue", ((IsTrueCriteria) criteria).getAttribute());
		} else if (criteria instanceof InListCriteria) {
			if (((InListCriteria) criteria).getAttribute() == null) {
				result.put("isin", convertSet(((InListCriteria) criteria).getIds()));
			} else {
				JSONObject sub = convertAttribute(criteria);
				sub.put("ids", convertSet(((InListCriteria) criteria).getIds()));
				result.put("isin", sub);
			}
		} else if (criteria instanceof IdGreaterStrictCriteria) {
			result.put("idgreater", ((IdGreaterStrictCriteria) criteria).getId());
		} else if (criteria instanceof IdGreaterThanCriteria) {
			result.put("idgreaterorequals", ((IdGreaterThanCriteria) criteria).getId());
		} else if (criteria instanceof IdLowerStrictCriteria) {
			result.put("idlower", ((IdLowerStrictCriteria) criteria).getId());
		} else if (criteria instanceof IdLowerThanCriteria) {
			result.put("idlowerorequals", ((IdLowerThanCriteria) criteria).getId());
		} else if (criteria instanceof IdEqualCriteria) {
			result.put("idequal", ((IdEqualCriteria) criteria).getId());
		} else if (criteria instanceof EqualCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((EqualCriteria) criteria).getValue() != null) {
				sub.put("value", ((EqualCriteria) criteria).getValue());
			}
			if (((EqualCriteria) criteria).getIntval() != null) {
				sub.put("intval", ((EqualCriteria) criteria).getIntval());
			}
			if (!((EqualCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false");
			}
			result.put("equals", sub);
		} else if (criteria instanceof LowerThanCriteria) {
			result.put("lowerthan", convertAttribute(criteria).put("value", ((LowerThanCriteria) criteria).getValue()));
		} else if (criteria instanceof GreaterThanCriteria) {
			result.put("greaterthan", convertAttribute(criteria).put("value", ((GreaterThanCriteria) criteria).getValue()));
		} else if (criteria instanceof LowerStrictCriteria) {
			result.put("lowerstrict", convertAttribute(criteria).put("value", ((LowerStrictCriteria) criteria).getValue()));
		} else if (criteria instanceof GreaterStrictCriteria) {
			result.put("greaterstrict", convertAttribute(criteria).put("value", ((GreaterStrictCriteria) criteria).getValue()));
		} else if (criteria instanceof ContainCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((ContainCriteria) criteria).getValue());
			if (((ContainCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("contains", sub);
		} else if (criteria instanceof StartCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((StartCriteria) criteria).getValue());
			if (((StartCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("starts", sub);
		} else if (criteria instanceof EndCriteria) {
			JSONObject sub = convertAttribute(criteria).put("value", ((EndCriteria) criteria).getValue());
			if (((EndCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("ends", sub);
		} else if (criteria instanceof LinkCriteria) {
			result.put("linkto", convertAttribute(criteria)
					.put("linkCode", ((LinkCriteria) criteria).getLinkCode())
					.put("id", ((LinkCriteria) criteria).getId()));
		} else if (criteria instanceof InGroupCriteria) {
			result.put("ingroup", convertAttribute(criteria)
					.put("type", ((InGroupCriteria) criteria).getType())
					.put("item", ((InGroupCriteria) criteria).getItem())
					.put("group", ((InGroupCriteria) criteria).getGroup()));
		} else if (criteria instanceof BeforeCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((BeforeCriteria) criteria).getValue() != null) {
				sub.put("value", ISODateFormater.toString(((BeforeCriteria) criteria).getValue()));
			}
			if (((BeforeCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true");
			}
			if (((BeforeCriteria) criteria).getYears() != 0) {
				sub.put("years", ((BeforeCriteria) criteria).getYears());
			}
			if (((BeforeCriteria) criteria).getMonths() != 0) {
				sub.put("months", ((BeforeCriteria) criteria).getMonths());
			}
			if (((BeforeCriteria) criteria).getDays() != 0) {
				sub.put("days", ((BeforeCriteria) criteria).getDays());
			}
			if (((BeforeCriteria) criteria).getHours() != 0) {
				sub.put("hours", ((BeforeCriteria) criteria).getHours());
			}
			if (((BeforeCriteria) criteria).getMinuts() != 0) {
				sub.put("minuts", ((BeforeCriteria) criteria).getMinuts());
			}
			result.put("before", sub);
		} else if (criteria instanceof AfterCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((AfterCriteria) criteria).getValue() != null) {
				sub.put("value", ISODateFormater.toString(((AfterCriteria) criteria).getValue()));
			}
			if (((AfterCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true");
			}
			if (((AfterCriteria) criteria).getYears() != 0) {
				sub.put("years", ((AfterCriteria) criteria).getYears());
			}
			if (((AfterCriteria) criteria).getMonths() != 0) {
				sub.put("months", ((AfterCriteria) criteria).getMonths());
			}
			if (((AfterCriteria) criteria).getDays() != 0) {
				sub.put("days", ((AfterCriteria) criteria).getDays());
			}
			if (((AfterCriteria) criteria).getHours() != 0) {
				sub.put("hours", ((AfterCriteria) criteria).getHours());
			}
			if (((AfterCriteria) criteria).getMinuts() != 0) {
				sub.put("minuts", ((AfterCriteria) criteria).getMinuts());
			}
			result.put("after", sub);
		} else if (criteria instanceof BetweenCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((BetweenCriteria) criteria).getBefore() != null) {
				sub.put("before", ISODateFormater.toString(((BetweenCriteria) criteria).getBefore()));
			}
			if (((BetweenCriteria) criteria).getAfter() != null) {
				sub.put("after", ISODateFormater.toString(((BetweenCriteria) criteria).getAfter()));
			}
			if (((BetweenCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true");
			}
			if (((BetweenCriteria) criteria).getAfteryears() != 0) {
				sub.put("afteryears", ((BetweenCriteria) criteria).getAfteryears());
			}
			if (((BetweenCriteria) criteria).getAftermonths() != 0) {
				sub.put("aftermonths", ((BetweenCriteria) criteria).getAftermonths());
			}
			if (((BetweenCriteria) criteria).getAfterdays() != 0) {
				sub.put("afterdays", ((BetweenCriteria) criteria).getAfterdays());
			}
			if (((BetweenCriteria) criteria).getAfterhours() != 0) {
				sub.put("afterhours", ((BetweenCriteria) criteria).getAfterhours());
			}
			if (((BetweenCriteria) criteria).getAfterminuts() != 0) {
				sub.put("afterminuts", ((BetweenCriteria) criteria).getAfterminuts());
			}
			if (((BetweenCriteria) criteria).getBeforeyears() != 0) {
				sub.put("beforeyears", ((BetweenCriteria) criteria).getBeforeyears());
			}
			if (((BetweenCriteria) criteria).getBeforemonths() != 0) {
				sub.put("beforemonths", ((BetweenCriteria) criteria).getBeforemonths());
			}
			if (((BetweenCriteria) criteria).getBeforedays() != 0) {
				sub.put("beforedays", ((BetweenCriteria) criteria).getBeforedays());
			}
			if (((BetweenCriteria) criteria).getBeforehours() != 0) {
				sub.put("beforehours", ((BetweenCriteria) criteria).getBeforehours());
			}
			if (((BetweenCriteria) criteria).getBeforeminuts() != 0) {
				sub.put("beforeminuts", ((BetweenCriteria) criteria).getBeforeminuts());
			}
			result.put("between", sub);
		} else if (criteria instanceof EqualICCriteria) {
			result.put("equalsic", convertAttribute(criteria).put("value", ((EqualICCriteria) criteria).getValue()));
		} else if (criteria instanceof HasRightCriteria) {
			JSONObject sub = convertAttribute(criteria).put("right", ((HasRightCriteria) criteria).getRight());
			if (((HasRightCriteria) criteria).getParam() != null) {
				sub.put("param", ((HasRightCriteria) criteria).getParam());
			}
			result.put("hasRight", sub);
		} else if (criteria instanceof CurrentUserCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((CurrentUserCriteria) criteria).getLinkCode() != null) {
				sub.put("linkcode", ((CurrentUserCriteria) criteria).getLinkCode());
			}
			if (((CurrentUserCriteria) criteria).getUserAttribute() != null) {
				sub.put("userAttribute", ((CurrentUserCriteria) criteria).getUserAttribute());
			}
			result.put("currentUser", sub);
		} else if (criteria instanceof PreGeneratedCriteria) {
			result.put("pregen", ((PreGeneratedCriteria) criteria).getSql());
		} else if (criteria instanceof SubstCriteria) {
			result.put("subst", ((SubstCriteria) criteria).getCode());
		} else if (criteria instanceof AttributeEqualsCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (!((AttributeEqualsCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false");
			}
			result.put("equalsAttributes", sub);
		} else if (criteria instanceof AttributeLowerCriteria) {
			result.put("lowerAttributes", convertAttribute(criteria));
		} else if (criteria instanceof AttributeLowerOrEqualsCriteria) {
			result.put("lowerEqualsAttributes", convertAttribute(criteria));
		} else if (criteria instanceof ChangedCriteria) {
			JSONObject sub = new JSONObject();
			if (((ChangedCriteria) criteria).getBefore() != null) {
				sub.put("before", ISODateFormater.toString(((ChangedCriteria) criteria).getBefore()));
			}
			if (((ChangedCriteria) criteria).getAfter() != null) {
				sub.put("after", ISODateFormater.toString(((ChangedCriteria) criteria).getAfter()));
			}
			if (((ChangedCriteria) criteria).isTrunc()) {
				sub.put("trunc", "true");
			}
			if (((ChangedCriteria) criteria).getAfteryears() != 0) {
				sub.put("afteryears", ((ChangedCriteria) criteria).getAfteryears());
			}
			if (((ChangedCriteria) criteria).getAftermonths() != 0) {
				sub.put("aftermonths", ((ChangedCriteria) criteria).getAftermonths());
			}
			if (((ChangedCriteria) criteria).getAfterdays() != 0) {
				sub.put("afterdays", ((ChangedCriteria) criteria).getAfterdays());
			}
			if (((ChangedCriteria) criteria).getAfterhours() != 0) {
				sub.put("afterhours", ((ChangedCriteria) criteria).getAfterhours());
			}
			if (((ChangedCriteria) criteria).getAfterminuts() != 0) {
				sub.put("afterminuts", ((ChangedCriteria) criteria).getAfterminuts());
			}
			if (((ChangedCriteria) criteria).getBeforeyears() != 0) {
				sub.put("beforeyears", ((ChangedCriteria) criteria).getBeforeyears());
			}
			if (((ChangedCriteria) criteria).getBeforemonths() != 0) {
				sub.put("beforemonths", ((ChangedCriteria) criteria).getBeforemonths());
			}
			if (((ChangedCriteria) criteria).getBeforedays() != 0) {
				sub.put("beforedays", ((ChangedCriteria) criteria).getBeforedays());
			}
			if (((ChangedCriteria) criteria).getBeforehours() != 0) {
				sub.put("beforehours", ((ChangedCriteria) criteria).getBeforehours());
			}
			if (((ChangedCriteria) criteria).getBeforeminuts() != 0) {
				sub.put("beforeminuts", ((ChangedCriteria) criteria).getBeforeminuts());
			}
			result.put("changed", sub);
		} else if (criteria instanceof DeletedCriteria) {
			result.put("deleted", ((DeletedCriteria) criteria).getAttribute());
		} else if (criteria instanceof UnlinkCriteria) {
			result.put("unlinkto", convertAttribute(criteria)
					.put("linkCode", ((UnlinkCriteria) criteria).getLinkCode())
					.put("id", ((UnlinkCriteria) criteria).getId()));
		} else if (criteria instanceof LinkEqualCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (!((EqualCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "false");
			}
			result.put("linkequals", sub);
		} else if (criteria instanceof LinkStartCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkStartCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("linkStarts", sub);
		} else if (criteria instanceof LinkEndCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkEndCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("linkEnds", sub);
		} else if (criteria instanceof LinkContainCriteria) {
			JSONObject sub = convertAttribute(criteria);
			if (((LinkContainCriteria) criteria).isCasesensitive()) {
				sub.put("cs", "true");
			}
			result.put("linkContains", sub);
		} else if (criteria instanceof LinkGreaterStrictCriteria) {
			result.put("linkgreaterstrict", convertAttribute(criteria));
		} else if (criteria instanceof LinkGreaterThanCriteria) {
			result.put("linkgreaterthan", convertAttribute(criteria));
		} else {
			throw new JSONException("Unexpected Criteria, unable to convert it in JSON:" + criteria);
		}
		return result;
	}

	private JSONObject convertAttribute(ISearchCriteria criteria) throws JSONException {
		JSONObject result = new JSONObject();
		if ((criteria instanceof IAttributeCriteria) && (((IAttributeCriteria) criteria).getAttribute() != null)) {
			result.put("attribute", ((IAttributeCriteria) criteria).getAttribute());
		}
		if ((criteria instanceof IAttributesCriteria) && (((IAttributesCriteria) criteria).getSecondAttribute() != null)) {
			result.put("secondAttribute", ((IAttributesCriteria) criteria).getSecondAttribute());
		}
		if (criteria instanceof AbstractLinkTestCriteria) {
			result.put("link", ((AbstractLinkTestCriteria) criteria).getLinkCode());
			result.put("value", ((AbstractLinkTestCriteria) criteria).getValue());
			if (((AbstractLinkTestCriteria) criteria).getReference() != null) {
				result.put("reference", ((AbstractLinkTestCriteria) criteria).getReference());
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
		case "or":
			return new OrCriteria(convert(((JSONArray) value)));
		case "and":
			return new AndCriteria(convert(((JSONArray) value)));
		case "not":
			return new NotCriteria(convert(((JSONObject) value)));
		case "constant":
			return new ConstantCriteria((value != null) && ("true".equalsIgnoreCase(value.toString()) || "yes".equalsIgnoreCase(value.toString()) || "1".equals(value.toString())));
		case "isnull":
		case "isnul":
		case "null":
		case "nul":
			return new IsNullCriteria((String) value);
		case "idequals":
		case "idequal":
		case "ideq":
		case "id":
			return new IdEqualCriteria((String) value);
		case "istrue":
			return new IsTrueCriteria((String) value);
		case "pregen":
		case "sql":
			return new PreGeneratedCriteria((String) value);
		case "subst":
		case "sub":
			return new SubstCriteria((String) value);
		case "deleted":
		case "del":
			return new DeletedCriteria((String) value);
		case "idgreaterstrict":
		case "idgreater":
		case "idgs":
			return new IdGreaterStrictCriteria((String) value);
		case "idgreaterorequals":
		case "idgreaterthan":
		case "idgt":
			return new IdGreaterThanCriteria((String) value);
		case "idlowerstrict":
		case "idlower":
		case "idls":
			return new IdLowerStrictCriteria((String) value);
		case "idlowerorequals":
		case "idlowerthan":
		case "idlt":
			return new IdLowerThanCriteria((String) value);
		case "isin":
		case "idin":
		case "in":
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
		case "equals":
		case "equal":
		case "eq":
			return fill(new EqualCriteria(), (JSONObject) value);
		case "linkgreaterstrict":
		case "linkgreater":
		case "lgreaterstrict":
		case "lgreater":
		case "lgs":
			return fill(new LinkGreaterStrictCriteria(), (JSONObject) value);
		case "linkgreaterthan":
		case "lgreaterthan":
		case "lgt":
			return fill(new LinkGreaterThanCriteria(), (JSONObject) value);
		case "lowerthan":
		case "lt":
			return fill(new LowerThanCriteria(), (JSONObject) value);
		case "greaterthan":
		case "gt":
			return fill(new GreaterThanCriteria(), (JSONObject) value);
		case "lowerstrict":
		case "lower":
		case "ls":
			return fill(new LowerStrictCriteria(), (JSONObject) value);
		case "greaterstrict":
		case "greater":
		case "gs":
			return fill(new GreaterStrictCriteria(), (JSONObject) value);
		case "contains":
		case "contain":
			return fill(new ContainCriteria(), (JSONObject) value);
		case "starts":
			return fill(new StartCriteria(), (JSONObject) value);
		case "ends":
			return fill(new EndCriteria(), (JSONObject) value);
		case "linkto":
		case "link":
		case "lto":
			return fill(new LinkCriteria(), (JSONObject) value);
		case "ingroup":
			return fill(new InGroupCriteria(), (JSONObject) value);
		case "before":
			return fill(new BeforeCriteria(), (JSONObject) value);
		case "after":
			return fill(new AfterCriteria(), (JSONObject) value);
		case "between":
			return fill(new BetweenCriteria(), (JSONObject) value);
		case "linkequals":
		case "linkequal":
		case "lequals":
		case "lequal":
		case "leq":
			return fill(new LinkEqualCriteria(), (JSONObject) value);
		case "equalsic":
		case "eqic":
			return fill(new EqualICCriteria(), (JSONObject) value);
		case "hasright":
		case "right":
			return fill(new HasRightCriteria(), (JSONObject) value);
		case "currentuser":
		case "user":
			return fill(new CurrentUserCriteria(), (JSONObject) value);
		case "equalsattributes":
		case "equalsa":
		case "eqa":
			return fill(new AttributeEqualsCriteria(), (JSONObject) value);
		case "lowerstrictattributes":
		case "lowerattributes":
		case "lsa":
			return fill(new AttributeLowerCriteria(), (JSONObject) value);
		case "lowerthanattributes":
		case "lowerequalsattributes":
		case "lowerequalsa":
		case "lowerthana":
		case "lowerequala":
		case "lta":
			return fill(new AttributeLowerOrEqualsCriteria(), (JSONObject) value);
		case "changed":
			return fill(new ChangedCriteria(), (JSONObject) value);
		case "unlinkto":
		case "unlink":
		case "unl":
			return fill(new UnlinkCriteria(), (JSONObject) value);
		case "linkstarts":
		case "linkstart":
		case "lstarts":
		case "lstart":
			return fill(new LinkStartCriteria(), (JSONObject) value);
		case "linkends":
		case "linkend":
		case "lends":
		case "lend":
			return fill(new LinkEndCriteria(), (JSONObject) value);
		case "linkcontains":
		case "linkcontain":
		case "lcontains":
		case "lcontain":
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
					case "attribute":
					case "att":
					case "a":
						if (criteria instanceof IAttributeCriteria) {
							((IAttributeCriteria) criteria).setAttribute(o.toString());
						}
						break;
					case "secondattribute":
					case "secatt":
					case "sa":
						if (criteria instanceof CurrentUserCriteria) {
							((CurrentUserCriteria) criteria).setUserAttribute(o.toString());
						}
						break;
					case "userattribute":
					case "useratt":
					case "uatt":
					case "ua":
						if (criteria instanceof IAttributeCriteria) {
							((IAttributeCriteria) criteria).setAttribute(o.toString());
						}
						break;
					case "linkcode":
					case "link":
					case "lc":
					case "l":
						if (criteria instanceof AbstractLinkTestCriteria) {
							((AbstractLinkTestCriteria) criteria).setLinkCode(o.toString());
						} else if (criteria instanceof LinkCriteria) {
							((LinkCriteria) criteria).setLinkCode(o.toString());
						} else if (criteria instanceof UnlinkCriteria) {
							((UnlinkCriteria) criteria).setLinkCode(o.toString());
						} else if (criteria instanceof CurrentUserCriteria) {
							((CurrentUserCriteria) criteria).setLinkCode(o.toString());
						}
						break;
					case "reference":
					case "ref":
					case "r":
						if (criteria instanceof AbstractLinkTestCriteria) {
							((AbstractLinkTestCriteria) criteria).setReference(o.toString());
						}
						break;
					case "value":
					case "val":
					case "v":
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
						} else if (criteria instanceof EqualICCriteria) {
							((EqualICCriteria) criteria).setValue(o.toString());
						} else if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setValue(getDate(o));
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setValue(getDate(o));
						}
						break;
					case "casesensitive":
					case "case":
					case "cs":
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
					case "intval":
					case "iv":
						int i = getInteger(o);
						if (criteria instanceof EqualCriteria) {
							((EqualCriteria) criteria).setIntval(i);
						}
						break;
					case "id":
						i = getInteger(o);
						if (criteria instanceof LinkCriteria) {
							((LinkCriteria) criteria).setId(i);
						} else if (criteria instanceof UnlinkCriteria) {
							((UnlinkCriteria) criteria).setId(i);
						}
						break;
					case "type":
						if (criteria instanceof InGroupCriteria) {
							((InGroupCriteria) criteria).setType(o.toString());
						}
						break;
					case "item":
						if (criteria instanceof InGroupCriteria) {
							((InGroupCriteria) criteria).setItem(o.toString());
						}
						break;
					case "group":
						if (criteria instanceof InGroupCriteria) {
							((InGroupCriteria) criteria).setGroup(o.toString());
						}
						break;
					case "trunc":
					case "tc":
					case "t":
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
					case "param":
					case "p":
						if (criteria instanceof HasRightCriteria) {
							((HasRightCriteria) criteria).setParam(o.toString());
						}
						break;
					case "right":
						i = getInteger(o);
						if (criteria instanceof HasRightCriteria) {
							((HasRightCriteria) criteria).setRight(i);
						}
						break;
					case "before":
						Date d = getDate(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBefore(d);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBefore(d);
						}
						break;
					case "after":
						d = getDate(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfter(d);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfter(d);
						}
						break;
					case "years":
					case "yr":
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setYears(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setYears(i);
						}
						break;
					case "months":
					case "mt":
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setMonths(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setMonths(i);
						}
						break;
					case "days":
					case "dy":
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setDays(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setDays(i);
						}
						break;
					case "hours":
					case "hr":
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setHours(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setHours(i);
						}
						break;
					case "minuts":
					case "mn":
						i = getInteger(o);
						if (criteria instanceof BeforeCriteria) {
							((BeforeCriteria) criteria).setMinuts(i);
						} else if (criteria instanceof AfterCriteria) {
							((AfterCriteria) criteria).setMinuts(i);
						}
						break;
					case "includelimits":
					case "limits":
						b = getBoolean(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setIncludeLimits(b);
						}
						break;
					case "afteryears":
					case "ayr":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfteryears(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfteryears(i);
						}
						break;
					case "aftermonths":
					case "amt":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAftermonths(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAftermonths(i);
						}
						break;
					case "afterdays":
					case "ady":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterdays(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterdays(i);
						}
						break;
					case "afterhours":
					case "ahr":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterhours(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterhours(i);
						}
						break;
					case "afterminuts":
					case "amn":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setAfterminuts(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setAfterminuts(i);
						}
						break;
					case "beforeyears":
					case "byr":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforeyears(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforeyears(i);
						}
						break;
					case "beforemonths":
					case "bmt":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforemonths(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforemonths(i);
						}
						break;
					case "beforedays":
					case "bdy":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforedays(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforedays(i);
						}
						break;
					case "beforehours":
					case "bhr":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforehours(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforehours(i);
						}
						break;
					case "beforeminuts":
					case "bmn":
						i = getInteger(o);
						if (criteria instanceof BetweenCriteria) {
							((BetweenCriteria) criteria).setBeforeminuts(i);
						} else if (criteria instanceof ChangedCriteria) {
							((ChangedCriteria) criteria).setBeforeminuts(i);
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
		return "true".equalsIgnoreCase(s) || "yes".equalsIgnoreCase(s) || "1".equals(s);
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