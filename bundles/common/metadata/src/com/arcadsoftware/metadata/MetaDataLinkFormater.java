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

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapFormater;
import com.arcadsoftware.beanmap.ITransientAttribute;

/**
 * This class wrap a BeanMapFormater to be able to format string related to a MetaDataLink.
 * 
 * <p>
 * The attributes code used into the format string can be either from the Source or the Target linked entity.
 * In case of name conflict you can prefix them with the corresponding entity type, or with "source." for
 * the source entity and "target." for the referenced one.
 * 
 * <p>
 * The tag %source.id% and %target.id% may be used to respectively get the Source of Target internal ID.
 * 
 * @author ARCAD Software
 * @see BeanMapFormater
 */
public class MetaDataLinkFormater implements ITransientAttribute {

	private final MetaDataLink link;
	private final BeanMapFormater formater;
	private final String sourcePrefix;
	private final String targetPrefix;
	private final ArrayList<ReferenceLine> sourceRefs;
	private final ArrayList<ReferenceLine> targetRefs;
	
	/**
	 * 
	 * @param link
	 */
	public MetaDataLinkFormater(MetaDataLink link) {
		super();
		this.link = link;
		sourcePrefix = link.getParent().getType() + '.';
		targetPrefix = link.getRefEntity().getType() + '.';
		sourceRefs = new ArrayList<ReferenceLine>();
		targetRefs = new ArrayList<ReferenceLine>();
		formater = new BeanMapFormater(link.getType()) {
			@Override
			protected String getAttributeCode(String code) {
				return MetaDataLinkFormater.this.getAttributeCode(super.getAttributeCode(code));
			}
		};
	}

	/**
	 * 
	 * @param formatString
	 * @param link
	 */
	public MetaDataLinkFormater(String formatString, MetaDataLink link) {
		this(link);
		setFormatString(formatString);
	}
	
	protected String getAttributeCode(String code) {
		if (code != null) {
			String clc = code.toLowerCase();
			if (clc.equals("source.id") || clc.equals("target.id") || clc.equals("source.type") || clc.equals("target.type")) {
				return clc;
			}
			if (clc.startsWith("source.")) { //$NON-NLS-1$
				ReferenceLine rl = link.getParent().getReferenceLine(code.substring(7));
				if (rl != null) {
					if (!sourceRefs.contains(rl)) {
						sourceRefs.add(rl);
					}
					return code;
				}
			}
			if (code.startsWith(sourcePrefix)) {
				ReferenceLine rl = link.getParent().getReferenceLine(code.substring(sourcePrefix.length()));
				if (rl != null) {
					if (!sourceRefs.contains(rl)) {
						sourceRefs.add(rl);
					}
					return code;
				}
			}
			if (clc.startsWith("target.")) { //$NON-NLS-1$
				ReferenceLine rl = link.getRefEntity().getReferenceLine(code.substring(7));
				if (rl != null) {
					if (!targetRefs.contains(rl)) {
						targetRefs.add(rl);
					}
					return code;
				}
			}
			if (code.startsWith(targetPrefix)) {
				ReferenceLine rl = link.getRefEntity().getReferenceLine(code.substring(targetPrefix.length()));
				if (rl != null) {
					if (!targetRefs.contains(rl)) {
						targetRefs.add(rl);
					}
					return code;
				}
			}
			ReferenceLine rl = link.getParent().getReferenceLine(code);
			if (rl != null) {
				if (!sourceRefs.contains(rl)) {
					sourceRefs.add(rl);
				}
				return code;
			}
			rl = link.getRefEntity().getReferenceLine(code);
			if (rl != null) {
				if (!targetRefs.contains(rl)) {
					targetRefs.add(rl);
				}
				return code;
			}
		}
		return null;
	}

	public void setFormatString(String formatString) {
		formater.setFormatString(formatString);
	}

	/**
	 * Format the BeanMaps attributes.
	 * 
	 * @param source
	 * @param target
	 * @return
	 */
	public String format(BeanMap source, BeanMap target) {
		BeanMap bean = new BeanMap();
		bean.addAll(target);
		bean.addAll(source);
		bean.addAll("target.", target); //$NON-NLS-1$
		bean.addAll("source.", source); //$NON-NLS-1$
		bean.addAll(targetPrefix, target);
		bean.addAll(sourcePrefix, source);
		bean.put("source.id", source.getId());
		bean.put("target.id", target.getId());
		bean.put("source.type", source.getType());
		bean.put("target.type", target.getType());
		return formater.format(bean);
	}
	
	/**
	 * Get the format string unparsed.
	 * 
	 * @return
	 */
	public String getFormatString() {
		return formater.getFormatString();
	}
	
	/**
	 * Get used source entity attributes into the format string.
	 * 
	 * <p>
	 * Note that theses attributes may not be attributes of the source Entity (as they may results from reference lines).
	 * 
	 * @return a non null list of attributes.
	 */
	public List<MetaDataAttribute> getSourceAttributes() {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(sourceRefs.size());
		for (ReferenceLine rl: sourceRefs) {
			if (rl.size() > 0) {
				result.add(rl.getLastAttribute());
			}
		}
		return result;
	}

	/**
	 * Get used source entity attributes into the format string.
	 * 
	 * @return a non null list of references.
	 */
	public List<ReferenceLine> getSourceReferenceLines() {
		return sourceRefs;
	}
	
	/**
	 * Get used source entity attribute codes into the format string.
	 * 
	 * <p>
	 * This list is not only the list of used codes into the the format string but declared attributes.
	 * 
	 * @return a non null String of codes.
	 */
	public String getSourceAttributeCodes() {
		StringBuilder sb = new StringBuilder();
		for (ReferenceLine rl: sourceRefs) {
			if (rl.size() > 0) {
				if (sb.length() > 0) {
					sb.append(' ');
				}
				sb.append(rl.getCode());
			}
		}
		return sb.toString();
	}
	
	/**
	 * Get used target entity attributes into the format string.
	 * 
	 * <p>
	 * Note that theses attributes may not be attributes of the target Entity (as they may results from reference lines).
	 * 
	 * @return a non null list of attributes.
	 */
	public List<MetaDataAttribute> getTargetAttributes() {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(targetRefs.size());
		for (ReferenceLine rl: targetRefs) {
			if (rl.size() > 0) {
				result.add(rl.getLastAttribute());
			}
		}
		return result;
	}

	/**
	 * Get used target entity attributes into the format string.
	 * 
	 * @return a non null list of references.
	 */
	public List<ReferenceLine> getTargetReferenceLines() {
		return targetRefs;
	}
	
	/**
	 * Get used target entity attribute codes into the format string.
	 * 
	 * <p>
	 * This list is not only the list of used codes into the the format string but declared attributes.
	 * 
	 * @return a non null String of codes.
	 */
	public String getTargetAttributeCodes() {
		StringBuilder sb = new StringBuilder();
		for (ReferenceLine rl: targetRefs) {
			if (rl.size() > 0) {
				if (sb.length() > 0) {
					sb.append(' ');
				}
				sb.append(rl.getCode());
			}
		}
		return sb.toString();
	}

	/**
	 * Return true if and only is the format string do not contain any reference to the source and target entities.
	 * 
	 * @return
	 */
	public boolean isStatic() {
		return sourceRefs.isEmpty() && targetRefs.isEmpty();
	}
	
	/**
	 * Return true if the given BeanMap contain all require references to thesource entity.
	 * @param source
	 * @return
	 */
	public boolean isSourceComplete(BeanMap source) {
		if (source != null) {
			for (ReferenceLine r: sourceRefs) {
				if (!source.contains(r.getCode())) {
					return false;
				}
			}
		}
		return true;
	}
	
	/**
	 * Return true if the given BeanMap contain all require references to thesource entity.
	 * @param source
	 * @return
	 */
	public boolean isTargetComplete(BeanMap target) {
		if (target != null) {
			for (ReferenceLine r: targetRefs) {
				if (!target.contains(r.getCode())) {
					return false;
				}
			}
		}
		return true;
	}

}
