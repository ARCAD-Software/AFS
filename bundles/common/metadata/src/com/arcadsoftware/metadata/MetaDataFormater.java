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
package com.arcadsoftware.metadata;

import java.util.ArrayList;
import java.util.List;

import com.arcadsoftware.beanmap.BeanMapFormater;
import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.beanmap.ITransientAttribute;
import com.arcadsoftware.metadata.internal.Activator;

/**
 * The MetaDataFormater can be used to format a String according to values of a BeanMap.
 * 
 * <p>
 * Format are string with %attribute% keys, if the "attribute" is not found in the associated MetaDataEntity 
 * then the text "attribute" will stay in the final text.
 * 
 * <p>
 * If you want to use the % character place %% in the format string.
 * 
 * <p>
 * Parameters can be used with "|" (pipe) character. Some parameters can be repeated into the same key :
 * <ul>
 * <li>%att1|att2% if the attribute "att1" is not defined then the attribute "att2" will be used.
 * <li>%att|100% represent the maximal string length of the attribute representation. If it is longer it will be
 * truncated.
 * <li>%att|-100% will add white spaces at the end of the attribute if its string representation is lower than 100
 * characters.
 * <li>%att|a text% will use "a text" as place holder if the given attribute does not exist. Note: "a text" must not be
 * a potential attribute text.
 * <li>%|% will be replaced by the pipe character.
 * </ul>
 * 
 * <p>
 * "%id%" may be used to be replaced by the BeanMap internal ID.
 * 
 * <p>
 * For instance theses strings are valid formats :
 * 
 * <ul>
 * <li>"[%code%] %name%"
 * <li>"[%code%] %name|text|80%"
 * <li>"[%code%] %name|text|80|Default String%"
 * <li>"%this is not an% attribute% but there% is invisible %% everywhere."
 * </ul>
 * 
 * @author ARCAD Software
 * @see BeanMapFormater
 */
public class MetaDataFormater extends BeanMapFormater implements ITransientAttribute {

	private final MetaDataEntity entity;
	private final ArrayList<ReferenceLine> refs;
	
	public MetaDataFormater(MetaDataEntity entity) {
		super(entity.getType());
		this.entity = entity;
		refs = new ArrayList<ReferenceLine>();
	}
	
	public MetaDataFormater(String formatString, MetaDataEntity entity) {
		this(entity);
		setFormatString(formatString);
	}

	@Override
	protected String getAttributeCode(String code) {
		code = super.getAttributeCode(code);
		if (code != null) {
			if (code.equalsIgnoreCase("id") || code.equalsIgnoreCase("type")) {
				return code;
			}
			ReferenceLine rl = entity.getReferenceLine(code);
			if (rl == null) {
				Activator.getInstance().info(String.format("The format code \"%s\" does not resolve as an Entity attribute.", code));
				return null;
			}
			if (rl.getLast() == null) {
				Activator.getInstance().info(String.format("One of the elements of the format code \"%s\", after the first one, does not resolve as an Entity attribute.", code));
				return null;
			}
			if (!refs.contains(rl)) {
				refs.add(rl);
			}
		}
		return code;
	}
	
	/**
	 * Get used attributes into the format string.
	 * 
	 * <p>
	 * Note that theses attributes may not be attributes of the referenced Entity (as they may results from reference lines).
	 * @return a non null list of attributes.
	 */
	public List<MetaDataAttribute> getAttributes() {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(refs.size());
		for (ReferenceLine rl: refs) {
			if (rl.size() > 0) {
				result.add(rl.getLastAttribute());
			}
		}
		return result;
	}

	/**
	 * Get used attributes into the format string.
	 * 
	 * @return a non null list of references.
	 */
	public List<ReferenceLine> getReferenceLines() {
		return refs;
	}
	
	/**
	 * Get used attribute codes into the format string.
	 * 
	 * <p>
	 * This list is not only the list of used codes into the the format string but declared attributes.
	 * 
	 * @return a non null String of codes.
	 */
	public String getAttributeCodes() {
		StringBuilder sb = new StringBuilder();
		for(ReferenceLine rl:refs) {
			if (rl.size() > 0) {
				if (sb.length() > 0) {
					sb.append(' ');
				}
				sb.append(rl.getCode());
			}
		}
		return sb.toString();
	}

	@Override
	protected String convertValue(IBeanMap item, String code, Object value, int limit, int completed) {
		if (code.equalsIgnoreCase("id")) {
			return Integer.toString(item.getId());
		}
		if (code.equalsIgnoreCase("type")) {
			return item.getType();
		}
		return super.convertValue(item, code, value, limit, completed);
	}
	
	
}
