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

import com.arcadsoftware.metadata.criteria.AbstractLinkTestCriteria;
import com.arcadsoftware.metadata.criteria.AbstractStringSearchCriteria;
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
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.IdGreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.IdInListCriteria;
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
import com.arcadsoftware.metadata.criteria.LinkLowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LinkLowerThanCriteria;
import com.arcadsoftware.metadata.criteria.LinkStartCriteria;
import com.arcadsoftware.metadata.criteria.LowerStrictCriteria;
import com.arcadsoftware.metadata.criteria.LowerThanCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.criteria.PreGeneratedCriteria;
import com.arcadsoftware.metadata.criteria.StartCriteria;
import com.arcadsoftware.metadata.criteria.SubstCriteria;
import com.arcadsoftware.metadata.criteria.UnlinkCriteria;
import com.arcadsoftware.metadata.internal.xml.ConstantCriteriaConverter;
import com.arcadsoftware.metadata.internal.xml.InListCriteriaConverter;
import com.arcadsoftware.metadata.internal.xml.NotCriteriaConverter;
import com.arcadsoftware.metadata.internal.xml.PregeneratedCriteriaConverter;
import com.arcadsoftware.rest.XStreamCompact;

/**
 * XStream serializer specialized to Criteria.
 */
public class XmlCriteriaStream extends XStreamCompact {
	
	protected static XStreamCompact initialize(XStreamCompact xs) {
		// Build all the necessary aliases...
		xs.alias("or", OrCriteria.class); //$NON-NLS-1$
		xs.addImplicitCollection(OrCriteria.class, "criterias"); //$NON-NLS-1$
		xs.alias("and", AndCriteria.class); //$NON-NLS-1$
		xs.addImplicitCollection(AndCriteria.class, "criterias"); //$NON-NLS-1$
		xs.alias("not", NotCriteria.class); //$NON-NLS-1$
		xs.registerConverter(new NotCriteriaConverter(xs.getMapper()));
		xs.alias("true", ConstantCriteria.class); //$NON-NLS-1$
		xs.alias("false", ConstantCriteria.class); //$NON-NLS-1$
		xs.alias("constant", ConstantCriteria.class); //$NON-NLS-1$
		xs.registerConverter(new ConstantCriteriaConverter());
		xs.alias("isnull", IsNullCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IsNullCriteria.class, "attribute"); //$NON-NLS-1$
		xs.alias("equals", EqualCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(EqualCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(EqualCriteria.class, "value"); //$NON-NLS-1$
		xs.useAttributeFor(EqualCriteria.class, "intval"); //$NON-NLS-1$
		xs.alias("lowerthan", LowerThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LowerThanCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(LowerThanCriteria.class, "value"); //$NON-NLS-1$
		xs.alias("greaterthan", GreaterThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(GreaterThanCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(GreaterThanCriteria.class, "value"); //$NON-NLS-1$
		xs.alias("lowerstrict", LowerStrictCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LowerStrictCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(LowerStrictCriteria.class, "value"); //$NON-NLS-1$
		xs.alias("greaterstrict", GreaterStrictCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(GreaterStrictCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(GreaterStrictCriteria.class, "value"); //$NON-NLS-1$
		xs.alias("contains", ContainCriteria.class); //$NON-NLS-1$
		xs.alias("starts", StartCriteria.class); //$NON-NLS-1$
		xs.alias("ends", EndCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(AbstractStringSearchCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractStringSearchCriteria.class, "value"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractStringSearchCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.alias("linkto", LinkCriteria.class); //$NON-NLS-1$ // Changed from "link" to "linkto" in version 2.0.0
		xs.useAttributeFor(LinkCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(LinkCriteria.class, "linkCode"); //$NON-NLS-1$
		xs.useAttributeFor(LinkCriteria.class, "id"); //$NON-NLS-1$
		xs.alias("idequal", IdEqualCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IdEqualCriteria.class, "id"); //$NON-NLS-1$
		xs.alias("before", BeforeCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "value"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "trunc"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "years"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "months"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "days"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "hours"); //$NON-NLS-1$
		xs.useAttributeFor(BeforeCriteria.class, "minuts"); //$NON-NLS-1$
		xs.alias("after", AfterCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "value"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "trunc"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "years"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "months"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "days"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "hours"); //$NON-NLS-1$
		xs.useAttributeFor(AfterCriteria.class, "minuts"); //$NON-NLS-1$
		xs.alias("between", BetweenCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "before"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "after"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "trunc"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "includeLimits"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "afteryears"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "aftermonths"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "afterdays"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "afterhours"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "afterminuts"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "beforeyears"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "beforemonths"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "beforedays"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "beforehours"); //$NON-NLS-1$
		xs.useAttributeFor(BetweenCriteria.class, "beforeminuts"); //$NON-NLS-1$
		xs.alias("istrue", IsTrueCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IsTrueCriteria.class, "attribute"); //$NON-NLS-1$
		xs.alias("linkEquals", LinkEqualCriteria.class); //$NON-NLS-1$
		xs.alias("linkequals", LinkEqualCriteria.class); //$NON-NLS-1$
		xs.alias("equalsIC", EqualICCriteria.class); //$NON-NLS-1$
		xs.alias("equalsic", EqualICCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(EqualICCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(EqualICCriteria.class, "value"); //$NON-NLS-1$
		xs.alias("hasRight", HasRightCriteria.class); //$NON-NLS-1$
		xs.alias("hasright", HasRightCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(HasRightCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(HasRightCriteria.class, "right"); //$NON-NLS-1$
		xs.useAttributeFor(HasRightCriteria.class, "param"); //$NON-NLS-1$
		xs.alias("currentUser", CurrentUserCriteria.class); //$NON-NLS-1$
		xs.alias("currentuser", CurrentUserCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(CurrentUserCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(CurrentUserCriteria.class, "userAttribute"); //$NON-NLS-1$
		xs.alias("pregen", PreGeneratedCriteria.class); //$NON-NLS-1$
		xs.registerConverter(new PregeneratedCriteriaConverter());
		xs.alias("subst", SubstCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(SubstCriteria.class, "code"); //$NON-NLS-1$
		xs.useAttributeFor(LinkEqualCriteria.class, "reference"); //$NON-NLS-1$
		xs.alias("equalsAttributes", AttributeEqualsCriteria.class); //$NON-NLS-1$
		xs.alias("equalsattributes", AttributeEqualsCriteria.class); //$NON-NLS-1$
		xs.alias("equalsa", AttributeEqualsCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(AttributeEqualsCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AttributeEqualsCriteria.class, "secondAttribute"); //$NON-NLS-1$
		xs.alias("lowerAttributes", AttributeLowerCriteria.class); //$NON-NLS-1$
		xs.alias("lowerattributes", AttributeLowerCriteria.class); //$NON-NLS-1$
		xs.alias("lowera", AttributeLowerCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(AttributeLowerCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AttributeLowerCriteria.class, "secondAttribute"); //$NON-NLS-1$
		xs.alias("lowerEqualsAttributes", AttributeLowerOrEqualsCriteria.class); //$NON-NLS-1$
		xs.alias("lowerequalsattributes", AttributeLowerOrEqualsCriteria.class); //$NON-NLS-1$
		xs.alias("lowerequalsa", AttributeLowerOrEqualsCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(AttributeLowerOrEqualsCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AttributeLowerOrEqualsCriteria.class, "secondAttribute"); //$NON-NLS-1$
		xs.alias("changed", ChangedCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "before"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "after"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "trunc"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "afteryears"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "aftermonths"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "afterdays"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "afterhours"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "afterminuts"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "beforeyears"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "beforemonths"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "beforedays"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "beforehours"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "beforeminuts"); //$NON-NLS-1$
		// New Criteria of version 1.0.1
		xs.alias("deleted", DeletedCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(DeletedCriteria.class, "attribute"); //$NON-NLS-1$
		// New criteria of version 1.0.2
		xs.alias("isIn", IdInListCriteria.class); //$NON-NLS-1$
		xs.alias("isin", IdInListCriteria.class); //$NON-NLS-1$
		xs.alias("isIn", InListCriteria.class); //$NON-NLS-1$
		xs.alias("isin", InListCriteria.class); //$NON-NLS-1$
		xs.registerConverter(new InListCriteriaConverter());
		// New criteria of version 1.0.3
		xs.alias("unlinkto", UnlinkCriteria.class); //$NON-NLS-1$ // Changed from "link" to "linkto" in version 2.0.0
		xs.useAttributeFor(UnlinkCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(UnlinkCriteria.class, "linkCode"); //$NON-NLS-1$
		xs.useAttributeFor(UnlinkCriteria.class, "id"); //$NON-NLS-1$
		// New criteria of version 1.5.1
		xs.useAttributeFor(CurrentUserCriteria.class, "linkCode"); //$NON-NLS-1$
		// New criteria of version 1.6.0
		xs.useAttributeFor(AbstractLinkTestCriteria.class, "linkCode"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractLinkTestCriteria.class, "attribute"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractLinkTestCriteria.class, "value"); //$NON-NLS-1$
		xs.useAttributeFor(LinkEqualCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.useAttributeFor(LinkEqualCriteria.class, "secondAttribute"); //$NON-NLS-1$
		xs.alias("linkStarts", LinkStartCriteria.class); //$NON-NLS-1$
		xs.alias("linkstarts", LinkStartCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LinkStartCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.alias("linkEnds", LinkEndCriteria.class); //$NON-NLS-1$
		xs.alias("linkends", LinkEndCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LinkEndCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.alias("linkContains", LinkContainCriteria.class); //$NON-NLS-1$
		xs.alias("linkcontains", LinkContainCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LinkContainCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.alias("linkgreaterstrict", LinkGreaterStrictCriteria.class); //$NON-NLS-1$
		xs.alias("linkgreaterthan", LinkGreaterThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(EqualCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.useAttributeFor(AttributeEqualsCriteria.class, "casesensitive"); //$NON-NLS-1$
		xs.aliasAttribute("a", "attribute"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("sa", "secondAttribute"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("secondattribute", "secondAttribute"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("v", "value"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("caseSensitive", "casesensitive"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("cs", "casesensitive"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.aliasAttribute("case", "casesensitive"); //$NON-NLS-1$ //$NON-NLS-2$
		xs.alias("idgreater", IdGreaterStrictCriteria.class); //$NON-NLS-1$
		xs.alias("idgreaterstrict", IdGreaterStrictCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IdGreaterStrictCriteria.class, "id"); //$NON-NLS-1$
		xs.alias("idgreaterorequals", IdGreaterThanCriteria.class); //$NON-NLS-1$
		xs.alias("idgreaterthan", IdGreaterThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IdGreaterThanCriteria.class, "id"); //$NON-NLS-1$
		xs.alias("idlower", IdLowerStrictCriteria.class); //$NON-NLS-1$
		xs.alias("idlowerstrict", IdLowerStrictCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IdLowerStrictCriteria.class, "id"); //$NON-NLS-1$
		xs.alias("idlowerorequals", IdLowerThanCriteria.class); //$NON-NLS-1$
		xs.alias("idlowerthan", IdLowerThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(IdLowerThanCriteria.class, "id"); //$NON-NLS-1$
		xs.useAttributeFor(ChangedCriteria.class, "attribute"); //$NON-NLS-1$
		// New criteria of version 2.0.0
		xs.alias("linkgreaterstrict", LinkLowerStrictCriteria.class); //$NON-NLS-1$
		xs.alias("linkgreaterthan", LinkLowerThanCriteria.class); //$NON-NLS-1$
		xs.useAttributeFor(LinkCriteria.class, "ignoreSubdivision"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractLinkTestCriteria.class, "ignoreSubdivision"); //$NON-NLS-1$
		xs.useAttributeFor(UnlinkCriteria.class, "ignoreSubdivision"); //$NON-NLS-1$
		xs.useAttributeFor(LinkCriteria.class, "deleted"); //$NON-NLS-1$
		xs.useAttributeFor(AbstractLinkTestCriteria.class, "deleted"); //$NON-NLS-1$
		xs.useAttributeFor(UnlinkCriteria.class, "deleted"); //$NON-NLS-1$
		xs.aliasAttribute("ignoreSubdivision", "nosub");
		xs.aliasAttribute("ignoresubdivision", "nosub");
		xs.aliasAttribute("linkcode", "linkCode");
		// For upcomming declarations, do not forget to add the dedicated mapping in the JsonCriteriaStream class.
		return xs;
	}
	
	public XmlCriteriaStream() {
		this(XmlCriteriaStream.class.getClassLoader());
	}
	
	public XmlCriteriaStream(ClassLoader classLoader) {
		super(classLoader);
		setMode(NO_REFERENCES);
		initialize(this);
	}

	@Override
	public Object fromXML(String xml) {
		return super.fromXML(xml);
	}

	@Override
	public String toXML(Object object) {
		return super.toXML(object);
	}

}
