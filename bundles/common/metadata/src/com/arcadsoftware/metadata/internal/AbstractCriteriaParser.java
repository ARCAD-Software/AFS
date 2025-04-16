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
package com.arcadsoftware.metadata.internal;

import java.text.ParseException;
import java.util.Date;
import java.util.List;

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
import com.arcadsoftware.metadata.criteria.GreaterStrictCriteria;
import com.arcadsoftware.metadata.criteria.GreaterThanCriteria;
import com.arcadsoftware.metadata.criteria.HasRightCriteria;
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
import com.arcadsoftware.metadata.criteria.natural.Token;
import com.arcadsoftware.osgi.ISODateFormater;

public abstract class AbstractCriteriaParser {

	public AbstractCriteriaParser() {
		super();
	}

	protected int integer(Token t) {
		if (t == null) {
			return 0;
		}
		try {
			return Integer.parseInt(t.image);
		} catch (NumberFormatException e) {
			throw new RuntimeException("Criteria Parsing Error: \"" + t.image + "\" should be an integer.");
		}
	}

	protected String string(Token t) {
		if (t == null) {
			return null;
		}
		String s = t.image;
		if (s.isEmpty() || (s.length() == 1)) {
			return s;
		}
		if (s.charAt(0) == '"') {
			return s.substring(1, s.length() - 1).replace("\"\"", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (s.charAt(0) == '\'') {
			return s.substring(1, s.length() - 1).replace("''", "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return s;
	}
	
	protected Date date(Token t) {
		if (t == null) {
			return null;
		}
		return date(t.image);
	}
	
	protected Date date(String d) {
		if (d == null) {
			return null;
		}
		if (d.length() == 10) {
			d += "T00:00:00.0000Z"; //$NON-NLS-1$
		} else if (d.length() == 16) {
			d += ":00.0000Z"; //$NON-NLS-1$
		} else if (d.length() == 19) {
			d += ".0000Z"; //$NON-NLS-1$
		}
		try {
			return ISODateFormater.toDate(d);
		} catch (ParseException e) {
			throw new RuntimeException("Criteria Parsing Error: \"" + d + "\" should be a valid ISO date format.");
		}
	}
	
	protected String dateString(Token t) {
		return ISODateFormater.toString(date(t));
	}
	
	protected ISearchCriteria constant(boolean value) {
		return new ConstantCriteria(value);
	}

	protected ISearchCriteria not(ISearchCriteria c) {
		return new NotCriteria(c);
	}

	protected ISearchCriteria or(ISearchCriteria... c) {
		return new OrCriteria(c);
	}

	protected ISearchCriteria and(ISearchCriteria... c) {
		return new AndCriteria(c);
	}

	protected ISearchCriteria equals(Token rl, int v) {
		return new EqualCriteria(rl.image, v);
	}

	protected ISearchCriteria equals(Token rl, String v, boolean cs) {
		return new EqualCriteria(rl.image, null, v, cs);
	}

	protected ISearchCriteria equals(Token rl1, Token rl2, boolean cs) {
		return new AttributeEqualsCriteria(rl1.image, rl2.image, cs);
	}

	protected ISearchCriteria equals(Token rl, Date v) {
		return new EqualCriteria(rl.image, ISODateFormater.toString(v));
	}

	protected ISearchCriteria comp(int op, Token rl, String v) {
		switch (op) {
		case 0: // <
			return new LowerStrictCriteria(rl.image, v);
		case 1: // >
			return new GreaterStrictCriteria(rl.image, v);
		case 2: // <=
			return new LowerThanCriteria(rl.image, v);
		case 3: // >=
			return new GreaterThanCriteria(rl.image, v);
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown comparison operator.");
	}

	protected ISearchCriteria comp(int op, Token rl1, Token rl2) {
		switch (op) {
		case 0: // <
			return new AttributeLowerCriteria(rl1.image, rl2.image);
		case 1: // >
			throw new RuntimeException("Criteria Parsing Error: Unimplemented Criteria (attributeA > attributeB), use (not attributeB <= attributeA).");
		case 2: // <=
			return new AttributeLowerOrEqualsCriteria(rl1.image, rl2.image);
		case 3: // >=
			throw new RuntimeException("Criteria Parsing Error: Unimplemented Criteria (attributeA >= attributeB), use (not attributeB < attributeA).");
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown comparison operator.");
	}

	protected ISearchCriteria strcmp(int op, Token rl1, String v, boolean cs) {
		switch (op) {
		case 0: // contains
			return new ContainCriteria(rl1.image, v, cs);
		case 1: // starts
			return new StartCriteria(rl1.image, v, cs);
		case 2: // ends
			return new EndCriteria(rl1.image, v, cs);
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown string comparison operator.");
	}
	
	protected ISearchCriteria isnull(Token rl) {
		return new IsNullCriteria(rl.image);
	}
	
	protected ISearchCriteria istrue(Token rl) {
		return new IsTrueCriteria(rl.image);
	}
	
	protected ISearchCriteria datecmp(int op, Token rl, Date x, boolean u, DateUnitList d) {
		switch (op) {
		case 0: // before
			return new BeforeCriteria(rl.image, x, u, d.getYear(), d.getMonth(), d.getDay(), d.getHour(), d.getMinute());
		case 1: // after
			return new AfterCriteria(rl.image, x, u, d.getYear(), d.getMonth(), d.getDay(), d.getHour(), d.getMinute());
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown date comparison operator.");
	}
	
	protected ISearchCriteria datecmp(int op, Token rl, Date x, boolean u, boolean l, DateUnitList a, Date z, DateUnitList b) {
		String ref = null;
		if (rl != null) {
			ref = rl.image;
		}
		switch(op ) {
		case 0:
			return new BetweenCriteria(ref, z, x, u, l, b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute(), a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute());
		case 1:
			return new BetweenCriteria(ref, x, z, u, l, a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute(), b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute());
		case -1:
			if (ref != null) {
				return new ChangedCriteria(ref.substring(0, ref.length() - 6), x, z, u, a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute(), b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute());
			}
			return new ChangedCriteria(x, z, u, a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute(), b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute());
		case -2:
			if (ref != null) {
				return new ChangedCriteria(ref.substring(0, ref.length() - 6), z, x, u, b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute(), a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute());
			}
			return new ChangedCriteria(z, x, u, b.getYear(), b.getMonth(), b.getDay(), b.getHour(), b.getMinute(), a.getYear(), a.getMonth(), a.getDay(), a.getHour(), a.getMinute());
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown date comparison operator.");
	}
	
	protected ISearchCriteria id(int op, int value) {
		switch (op) {
		case 0: // equals
			return new IdEqualCriteria(value);
		case 1: // greater
			return new IdGreaterStrictCriteria(value);
		case 2: // greater or equals
			return new IdGreaterThanCriteria(value);
		case 3: // lower
			return new IdLowerStrictCriteria(value);
		case 4: // lower or equals
			return new IdLowerThanCriteria(value);
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown ID comparison operator.");
	}
	
	protected ISearchCriteria link(String linkCode, Token rl, int value) {
		if (rl != null) {
			return new LinkCriteria(value, linkCode, rl.image);
		}
		return new LinkCriteria(value, linkCode);
	}
	
	protected ISearchCriteria link(int op, String linkCode, Token rl, Token al, Token al2, String v, boolean cs) {
		String ref = null;
		if (rl != null) {
			ref = rl.image;
		}
		String att = null;
		if (al != null) {
			att = al.image;
		}
		String att2 = null;
		if (al2 != null) {
			att2 = al2.image;
		}
		switch (op) {
		case 0: // equals
			return new LinkEqualCriteria(ref, linkCode, att, att2, v, cs);
		case 1: // greater
			return new LinkGreaterStrictCriteria(ref, linkCode, att, v);
		case 2: // greater or equals
			return new LinkGreaterThanCriteria(ref, linkCode, att, v);
		case 3: // contains
			return new LinkContainCriteria(ref, linkCode, att, v, cs);
		case 4: // starts
			return new LinkStartCriteria(ref, linkCode, att, v, cs);
		case 5: // ends
			return new LinkEndCriteria(ref, linkCode, att, v, cs);
		case 6: // greater
			return new LinkLowerStrictCriteria(ref, linkCode, att, v);
		case 7: // greater or equals
			return new LinkLowerThanCriteria(ref, linkCode, att, v);
		}
		throw new RuntimeException("Criteria Parsing Error: Unknown link comparison operator.");
	}
	
	protected ISearchCriteria unlink(String linkCode, Token rl, int value) {
		if (rl != null) {
			return new NotCriteria(new LinkCriteria(value, linkCode, rl.image));
		}
		return new NotCriteria(new LinkCriteria(value, linkCode));
	}
	
	protected ISearchCriteria hasRight(String att, Token r, Token p) {
		Integer right = null;
		if (r != null) {
			right = integer(r);
		}
		if (p == null) {
			return new HasRightCriteria(att, right, null);
		}
		try {
			return new HasRightCriteria(att, right, Integer.parseInt(p.image));
		} catch (NumberFormatException e) {
			return new HasRightCriteria(att, right, p.image);
		}
	}
	
	protected ISearchCriteria currentUser(Token rl, Token l, Token t) {
		String ua = null;
		if (rl != null) {
			ua = rl.image;
		}
		String lc = null;
		if (l != null) {
			lc = l.image;
		}
		return new CurrentUserCriteria(t.image, lc, ua);
	}
	
	protected ISearchCriteria pregen(String sql) {
		return new PreGeneratedCriteria(sql);
	}
	
	protected ISearchCriteria subst(String code) {
		return new SubstCriteria(code);
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected ISearchCriteria isin(List vals) {
		return new InListCriteria(vals);
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected ISearchCriteria isin(Token rl, List vals) {
		return new InListCriteria(rl.image, vals);
	}
	
	protected ISearchCriteria deleted(Token rl) {
		if (rl != null) {
			String ref = rl.image;
			if (ref.toLowerCase().endsWith(".@deleted")) { //$NON-NLS-1$
				ref = ref.substring(0, ref.length() - 9);
			}
			return new DeletedCriteria(ref);
		}
		return new DeletedCriteria();
	}
	
	protected ISearchCriteria changedBy(String att, Token t) {
		if (".".equals(att)) { //$NON-NLS-1$
			att = null;
		}
		int id = 0;
		if (!t.image.equalsIgnoreCase("@user")) { //$NON-NLS-1$
			id = integer(t);
		}
		return new ChangedByCriteria(att, id);
	}
}
