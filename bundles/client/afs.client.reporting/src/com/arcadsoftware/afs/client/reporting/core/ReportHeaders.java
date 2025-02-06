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
package com.arcadsoftware.afs.client.reporting.core;

import java.util.Collections;
import java.util.Comparator;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.aev.core.collections.IArcadCollectionItem;
import com.arcadsoftware.afs.client.reporting.Activator;

public class ReportHeaders extends ArcadCollection {

	private class HeaderSorter implements Comparator<ReportHeader> {
		@Override
		public int compare(ReportHeader arg0, ReportHeader arg1) {

			try {
				if ((arg0 != null) && (arg1 != null)) {
					final String s0 = arg0.getCategory();
					final String s1 = arg1.getCategory();
					if ((s0 != null) && (s1 != null)) {
						if (s0.compareTo(s1) == 0) {
							final String n0 = arg0.getName();
							final String n1 = arg1.getName();
							if ((n0 != null) && (n1 != null)) {
								return n0.compareTo(n1);
							}
						} else {
							return s0.compareTo(s1);
						}
					}
				}
			} catch (final Exception e) {
				Activator.getDefault().error(e.getLocalizedMessage(), e);
			}
			return 0;
		}

	}

	public void add(ReportHeader c) {
		this.add((IArcadCollectionItem) c);
	}

	public ReportHeader itemAt(int index) {
		return (ReportHeader) items(index);
	}

	@SuppressWarnings("unchecked")
	public void sort() {
		Collections.sort(getList(), new HeaderSorter());
	}

	private int elementCount(String s, char c) {
		int count = 0;
		for (int i = 0; i < s.length(); i++) {
			if (s.charAt(i) == c) {
				count++;
			}
		}
		return count;
	}

	public static String substractString(String s, String stringToSubstract) {
		if (!s.startsWith(stringToSubstract)) {
			return s;
		} else {
			return s.substring(stringToSubstract.length());
		}
	}

	public void setLevels() {
		int currentLevel = 1;
		String oldCategory = ""; //$NON-NLS-1$
		for (int i = 0; i < count(); i++) {
			final ReportHeader h = itemAt(i);
			final String category = h.getCategory();
			if (category.equalsIgnoreCase(oldCategory)) {
				h.setLevel(currentLevel + 1);
			} else {
				currentLevel = elementCount(category, ReportCategory.CATEGORY_SEPARATOR) + 1;
				String s = substractString(category, oldCategory);
				if (s.equalsIgnoreCase(category)) {
					currentLevel = 1;
				}
				if (s.charAt(0) == ReportCategory.CATEGORY_SEPARATOR) {
					s = s.substring(1);
				}
				final String[] segments = s.split(String.valueOf(ReportCategory.CATEGORY_SEPARATOR));
				for (int j = 0; j < segments.length; j++) {
					insert(i + j, new ReportCategory(segments[j], currentLevel + j));
				}
				i = ((i + segments.length) - 1) + 1;
				currentLevel = (currentLevel + segments.length) - 1;
				h.setLevel(currentLevel + 1);
				oldCategory = category;
			}

		}
	}

}
