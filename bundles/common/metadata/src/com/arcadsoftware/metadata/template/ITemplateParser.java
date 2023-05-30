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
package com.arcadsoftware.metadata.template;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;

/**
 * This OSGi Service parse Template strings linked to one or two entity items.
 */
public interface ITemplateParser {

	public static final String clazz = ITemplateParser.class.getName();
	
	/**
	 * Test the given item.
	 * 
	 * @param item
	 * @param criteria
	 * @param language
	 * @return
	 */
	public boolean test(BeanMap item, ISearchCriteria criteria, Language language);

	/**
	 * 
	 * Test the given items.
	 * 
	 * @param item
	 * @param secondaryItem
	 * @param template
	 * @param language
	 * @return
	 */
	public boolean test(BeanMap item, BeanMap secondary, IConditionalTemplate template, Language language);

	/**
	 * Parse the given text.
	 * 
	 * @param item
	 * @param text
	 * @param language
	 * @return
	 */
	public String parse(BeanMap item, String text, Language language);

	/**
	 * Parse the given text.
	 * 
	 * <p>This version use a secondary bean, theses tags are prefixed with the bean type.
	 * 
	 * <p>The types of the both bean can be the same, only the second one need to be prefixed.
	 * 
	 * @param item
	 * @param secondary a secondary item (may be null).
	 * @param text
	 * @param language
	 * @return
	 */
	public String parse(BeanMap item, BeanMap secondary, String text, Language language);

	/**
	 * Parse the given template text.
	 * 
	 * <p>This version use a secondary bean, theses tags are prefixed with the bean type.
	 * 
	 * <p>The types of the both bean can be the same, only the second one need to be prefixed.
	 * 
	 * @param item
	 * @param secondary
	 * @param template
	 * @param language
	 * @return
	 */
	public String parseText(BeanMap item, BeanMap secondary, ITemplate template, Language language);
	
	/**
	 * Parse the given template subject.
	 * 
	 * <p>This version use a secondary bean, theses tags are prefixed with the bean type.
	 * 
	 * <p>The types of the both bean can be the same, only the second one need to be prefixed.
	 * 
	 * @param item
	 * @param secondary
	 * @param template
	 * @param language
	 * @return
	 */
	public String parseSubject(BeanMap item, BeanMap secondary, ITemplate template, Language language);
}
