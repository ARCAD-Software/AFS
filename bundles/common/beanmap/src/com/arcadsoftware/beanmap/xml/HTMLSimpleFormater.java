/*******************************************************************************
 * Copyright (c) 2023 ARCAD Software.
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
package com.arcadsoftware.beanmap.xml;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map.Entry;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;

/**
 * This basic utility tool class allow to generate simple HTML documents.
 * 
 * @author ARCAD Software
 */
public class HTMLSimpleFormater {

	private static final String XHTMLSTART1 = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n<title>"; //$NON-NLS-1$
	private static final String XHTMLSTART2 = "</title>\n</head>\n<body>\n"; //$NON-NLS-1$
	private static final String XHTMLEND = "</body></html>"; //$NON-NLS-1$

	public static void escapeHTML(StringBuilder sb, String value) {
		for(char c:value.toCharArray()) {
			switch (c) {
			case '<':
				sb.append("&lt;"); //$NON-NLS-1$
				break;
			case '>':
				sb.append("&gt;"); //$NON-NLS-1$
				break;
			case '\n':
				sb.append("<br/>"); //$NON-NLS-1$
				break;
			case '\t':
				sb.append("&nbsp;&nbsp;&nbsp;&nbsp;"); //$NON-NLS-1$
				break;
			case '"':
				sb.append("&quot;"); //$NON-NLS-1$			
				break;
			case '&':
				sb.append("&amp;"); //$NON-NLS-1$
				break;
			default:
				sb.append(c);
				break;
			}
		}
	}

	private StringBuilder sb;
	private boolean buildPage;
	
	public HTMLSimpleFormater() {
		super();
		sb = new StringBuilder();
	}
	
	public HTMLSimpleFormater(String pageTitle) {
		this();
		this.buildPage = true;
		sb.append(XHTMLSTART1);
		escapeHTML(sb, pageTitle);
		sb.append(XHTMLSTART2);
	}

	/**
	 * Consumer may implement this method to add HTTP link when weak references are stored into a BeanMap.
	 * 
	 * @param bean
	 * @param key
	 * @param value
	 * @return
	 */
	protected String getHRef(BeanMap bean, String key, String value) {
		return null;
	}
	
	/**
	 * Consumer may implement this method to add HTTP link to BeanMap details in lists.
	 * @param bean
	 * @return
	 */
	protected String getHRef(BeanMap bean) {
		return null;
	}
	
	public void append(String paragraph) {
		sb.append("<p>"); //$NON-NLS-1$
		escapeHTML(sb, paragraph);
		sb.append("</p>\n"); //$NON-NLS-1$
	}
	
	public void append(String key, Object value) {
		sb.append("<p><strong>"); //$NON-NLS-1$
		escapeHTML(sb, key);
		sb.append("</strong>: \n"); //$NON-NLS-1$
		escapeHTML(sb, value.toString());
		sb.append("</p>\n"); //$NON-NLS-1$
	}
	
	public void append(BeanMap bean) {
		sb.append("<h2>"); //$NON-NLS-1$
		escapeHTML(sb, bean.getType());
		sb.append("&nbsp;#"); //$NON-NLS-1$
		sb.append(bean.getId());
		sb.append("</h2>\n"); //$NON-NLS-1$
		if (bean.getDate() != null) {
			sb.append("<p>Modification: "); //$NON-NLS-1$
			sb.append(bean.getDate());
			sb.append("</p>\n"); //$NON-NLS-1$
		}
		if (bean.isDeleted()) {
			sb.append("<p><i>Is deleted.</i></p>\n"); //$NON-NLS-1$
		}
		sb.append("<ul>\n");
		for(Entry<String, Object> e:bean.entrySet()) {
			sb.append("<li><strong>"); //$NON-NLS-1$
			escapeHTML(sb, e.getKey());
			sb.append("</strong>: \n"); //$NON-NLS-1$
			if (e.getValue() != null) {
				String s = e.getValue().toString();
				String href = getHRef(bean, e.getKey(), s);
				if (href != null) {
					sb.append("<a href=\""); //$NON-NLS-1$
					sb.append(href);
					sb.append("\">"); //$NON-NLS-1$
				}
				escapeHTML(sb, s);
				if (href != null) {
					sb.append("</a>"); //$NON-NLS-1$
				}
			} else {
				sb.append("<i>NULL</i>"); //$NON-NLS-1$
			}
			sb.append("</li>\n"); //$NON-NLS-1$
		}
		sb.append("</ul>\n"); //$NON-NLS-1$
	}
	
	public void append(BeanMapList list) {
		HashSet<String> cols = new HashSet<String>();
		for(BeanMap bean: list) {
			cols.addAll(bean.keys());
		}
		append(list, cols);
	}
	
	public void append(BeanMapList list, Collection<String> cols) {
		sb.append("<table border=\"1\"><tr>\n"); //$NON-NLS-1$
		sb.append("<th>#</th>\n"); //$NON-NLS-1$
		for(String col:cols) {
			sb.append("<th>"); //$NON-NLS-1$
			escapeHTML(sb, col);
			sb.append("</th>\n"); //$NON-NLS-1$
		}
		for(BeanMap bean:list) {
			sb.append("</tr><tr>\n<td align=\"right\"><code><strong>"); //$NON-NLS-1$
			String href = getHRef(bean);
			if (href != null) {
				sb.append("<a href=\""); //$NON-NLS-1$
				sb.append(href);
				sb.append("\">"); //$NON-NLS-1$
			}
			sb.append(bean.getId());
			if (href != null) {
				sb.append("</a>"); //$NON-NLS-1$
			}
			sb.append("</strong></code></td>\n"); //$NON-NLS-1$
			for(String col:cols) {
				sb.append("<td>"); //$NON-NLS-1$
				Object v = bean.get(col);
				if (v != null) {
					String s = v.toString();
					href = getHRef(bean, col, s);
					if (href != null) {
						sb.append("<a href=\""); //$NON-NLS-1$
						sb.append(href);
						sb.append("\">"); //$NON-NLS-1$
					}
					escapeHTML(sb, v.toString());
					if (href != null) {
						sb.append("</a>"); //$NON-NLS-1$
					}
				}
				sb.append("</td>\n"); //$NON-NLS-1$
			}
		}
		sb.append("</tr></table>\n"); //$NON-NLS-1$
	}

	@Override
	public String toString() {
		if (buildPage) {
			return sb.toString() + XHTMLEND;
		}
		return sb.toString();
	}
	
}
