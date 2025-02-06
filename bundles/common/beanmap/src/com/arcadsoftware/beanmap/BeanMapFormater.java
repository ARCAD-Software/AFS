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
package com.arcadsoftware.beanmap;

import java.text.DateFormat;
import java.util.Date;

/**
 * Use some kind of string format with the given BeanMap attribute.
 * 
 * <p>
 * Format are string with %attribute% keys, if attribute is not found then the text "attribute" will stay in the final
 * text.
 * 
 * <p>
 * <b>Please Note</b> that this implementation not do made any assumption about the BeanMap data model (aka Entity).
 * Metadata projet provide an extended version of this formater that check that attributes codes are real ones and 
 * propose richer value converters.
 * 
 * <p>
 * If you want to use the % character place %% in the format string.
 * 
 * <p>
 * Parameters can be used with "|" (pipe) character. Some parameters can be repeated into the same key :
 * <ul>
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
 * For instance theses strings are valid formats :
 * 
 * <ul>
 * <li>"[%code%] %name%"
 * <li>"[%code%] %name|text|80%"
 * <li>"[%code%] %name|text|80|Default String%"
 * <li>"%this is not an% attribute% but there% is invisible %% everywhere."
 * </ul>
 */
public class BeanMapFormater {
	
	private String formatString;
	private String[] strings;
	private String[] attributes;
	private int[] limits;
	private int[] completes;
	private String type;
	private boolean allowTabulations;
	private boolean allowReferences = true;

	/**
	 * Construct a formatter.
	 * 
	 * @param type the BeanMap type to process.
	 */
	public BeanMapFormater(String type) {
		super();
		this.type = type;
	}

	/**
	 * Construct a formatter.
	 * 
	 * @param format the format string, this string is immediately parsed.
	 * @param type the BeanMap type to process.
	 * @param allowReferences true is references lines are allowed.
	 */
	public BeanMapFormater(String format, String type, boolean allowReferences) {
		this(type);
		this.allowReferences = allowReferences;
		setFormatString(format);
	}

	/**
	 * Define or modifie the format string.
	 * 
	 * @param format
	 */
	public void setFormatString(String format) {
		formatString = format;
		if ((format == null) || (format.length() == 0)) {
			strings = null;
			return;
		}
		strings = format.split("\\%"); //$NON-NLS-1$
		if ((strings == null) || (strings.length == 0)) {
			strings = null;
			return;
		}
		attributes = new String[strings.length];
		limits = new int[strings.length];
		completes = new int[strings.length];
		if (strings.length == 1) {
			return;
		}
		boolean att = true;
		for (int i = 1; i < strings.length; i++) {
			if ((strings[i] == null) || (strings[i].length() == 0)) {
				if (i < (strings.length - 1)) {
					strings[i] = "%"; //$NON-NLS-1$
				}
				att = !att;
			} else if (att) {
				int limit = 0;
				int complete = 0;
				attributes[i] = null;
				String dstring = ""; //$NON-NLS-1$
				for (String a : strings[i].split("\\|")) { //$NON-NLS-1$
					if ((a != null) && (a.length() > 0)) {
						try {
							int x = Integer.parseInt(a);
							if (attributes[i] != null) {
								if (x > 0) {
									limit = x;
								} else if (x < 0) {
									complete = -x;
								}
							}
						} catch (NumberFormatException e) {
							if (attributes[i] == null) {
								attributes[i] = getAttributeCode(a);
								/*
								// ignore references.
								if (attributes[i] != null) {
									if (attributes[i].isReference()) {
										attributes[i] = null;
									}
								}*/
							}
							dstring = a;
						}
					}
				}
				att = false;
				if (attributes[i] != null) {
					limits[i] = limit;
					completes[i] = complete;
					if (strings[i].indexOf('|') >= 0) {
						strings[i] = dstring;
					} else {
						strings[i] = ""; //$NON-NLS-1$
					}
				}
			} else {
				att = true;
			}
		}
	}

	/**
	 * Get the format string unparsed.
	 * 
	 * @return
	 */
	public String getFormatString() {
		return formatString;
	}
	
	/**
	 * Override this method to test the attribute code according to a model (e.g. MetadataEntity).
	 *
	 * <p>default implementation accept any codes.
	 * 
	 * @param code an attribute code.
	 * @return the real attribute code to get from the BeanMap, or null if this string does not represent any valid code.
	 */
	protected String getAttributeCode(String code) {
		if ((!allowReferences) && (code.indexOf('.') > -1)) {
			return null;
		}
		return code;
	}

	/**
	 * Format the BeanMap Attributes.
	 * 
	 * @param item
	 * @return a string, never return null.
	 */
	public String format(IBeanMap item) {
		if ((strings == null) || (strings.length == 0)) {
			if (item != null) {
				return item.toString();
			}
			return ""; //$NON-NLS-1$
		}
		if (strings.length == 1) {
			return strings[0];
		}
		if (item == null) {
			item = new BeanMap(getType());
		} else if ((item instanceof ITypedBean) && (!getType().equals(((ITypedBean) item).getType()))) {
			return item.toString();
		}
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < strings.length; i++) {
			if (attributes[i] != null) {
				Object o = item.get(attributes[i]);
				String s = convertValue(item, attributes[i], o, limits[i], completes[i]);
				if (s.length() != 0) {
					result.append(s);
				} else {
					result.append(strings[i]);
				}
			} else {
				result.append(strings[i]);
			}
		}
		return result.toString();
	}

	protected String convertValue(IBeanMap item, String code, Object value, int limit, int completed) {
		StringBuilder result = new StringBuilder();
		String s;
		if (value != null) {
			if (value instanceof Date) {
				s = DateFormat.getDateInstance(DateFormat.SHORT).format((Date) value);
			} else if (!allowTabulations) {
				s = value.toString().replace('\t', ' ');
			} else {
				s = value.toString();
			}
			if ((limit > 0) && (s.length() > limit)) {
				s = s.substring(0, limit);
			}
			result.append(s);
		} else {
			s = ""; //$NON-NLS-1$
		}
		for (int j = s.length(); j < completed; j++) {
			result.append(' ');
		}
		return result.toString();
	}

	/**
	 * Return true if the given BeanMap contain all the required value to generate the string.
	 * @param item
	 * @param ignoreDefaults if true the result will do not take into account any default value included into the format string.
	 * @return
	 */
	public boolean isComplete(BeanMap item, boolean ignoreDefaults) {
		// strings[0] is never an attribute...
		for (int i = 1; i < strings.length; i++) {
			if (attributes[i] != null) {
				if (!item.contains(attributes[i])) {
					if (ignoreDefaults || (strings[i] == null) || strings[i].isEmpty()) {
						return false;
					}
				}
			}
		}
		return true;
	}
	
	/**
	 * return the correct value for the defined Attribut without formatting it in string. This is useful to order columns of complex data types
	 * (cascading references to other types of entity)
	 *
	 * <p><b>Use compare instead of this method</b>
	 * 
	 * @param item
	 * @return value of the item for the predefined attributes
	 */
	@Deprecated
	public Object nonFormat(IBeanMap item) {
		return firstDiscriminant(item);
	}
	
	/**
	 * Return the first discriminant value of this formated string.
	 * 
	 * <p>
	 * The discriminant is the first value that can be different from two different items.
	 * 
	 * <p><b>Note that two discriminant can be equal even if the resulting (complete) formating string is different, thus using this
	 * value to order a list can produce errors.</b>  You should use <code>compare</code> function to order lists.
	 * 
	 * @param item
	 * @return
	 */
	public Object firstDiscriminant(IBeanMap item) {
		if ((strings == null) || (strings.length == 0)) {
			if (item != null) {
				return item.toString();
			}
			return ""; //$NON-NLS-1$
		}
		if (strings.length == 1) {
			return strings[0];
		}
		if (item == null) {
			item = new BeanMap(getType());
		} else if ((item instanceof ITypedBean) && (!getType().equals(((ITypedBean) item).getType()))) {
			return item.toString();
		}
		for (int i = 0; i < strings.length; i++) {
			if (attributes[i] != null) {
				return item.get(attributes[i]);
			}
		}
		return ""; //$NON-NLS-1$
	}

	/**
	 * Compare to value without formating the string. The real value are compared (not the formated ones) this may represent the real
	 * order value but with visual disorder (especially with Date values).
	 * 
	 * <p>Null values are always considered as lower than non null ones.
	 * 
	 * @param item1 a BeanMap
	 * @param item2 another BeanMap
	 * @return &lt;0 if item1 &lt; item2, =0 if item1 = item2, &gt; 0 if item1 &gt; item2.
	 * @see BeanMapFormater#compareText(IBeanMap, IBeanMap) 
	 */
	public int compare(IBeanMap item1, IBeanMap item2) {
		if (item1 == null) {
			if (item2 == null) {
				return 0;
			}
			return 1;
		}
		if (item2 == null) {
			return -1;
		}
		for (int i = 0; i < strings.length; i++) {
			if (attributes[i] != null) {
				Object o1 = item1.get(attributes[i]);
				Object o2 = item2.get(attributes[i]);
				if (o1 == null) {
					if (o2 != null) {
						return 1;
					}
				} else if (o2 == null) {
					return -1;
				} else if (!o1.equals(o2)) {
					if (o1 instanceof Comparable) {
						@SuppressWarnings({ "unchecked", "rawtypes" })
						int result = ((Comparable)o1).compareTo(o2);
						if (result != 0) {
							return result;
						}
					} else if (o2 instanceof Comparable) {
						@SuppressWarnings({ "unchecked", "rawtypes" })
						int result = ((Comparable)o2).compareTo(o1);
						if (result != 0) {
							return -result;
						}
					} else {
						int result = o1.toString().compareTo(o2.toString());
						if (result != 0) {
							return result;
						}
					}
				}
			}
		}
		return 0;
	}
	
	/**
	 * Compare to value without completely format the string.
	 * 
	 * <p>Null values are always considered as lower than non null ones.
	 * 
	 * @param item1 a BeanMap
	 * @param item2 another BeanMap
	 * @return &lt;0 if item1 &lt; item2, =0 if item1 = item2, &gt; 0 if item1 &gt; item2.
	 * @see BeanMapFormater#compare(IBeanMap, IBeanMap) 
	 */
	public int compareText(IBeanMap item1, IBeanMap item2) {
		if (item1 == null) {
			if (item2 == null) {
				return 0;
			}
			return 1;
		}
		if (item2 == null) {
			return -1;
		}
		for (int i = 0; i < strings.length; i++) {
			if (attributes[i] != null) {
				Object o1 = item1.get(attributes[i]);
				Object o2 = item2.get(attributes[i]);
				if (o1 == null) {
					if (o2 != null) {
						return 1;
					}
				} else if (o2 == null) {
					return -1;
				} else if (!o1.equals(o2)) {
					String s1 = convertValue(item1, attributes[i], o1, limits[i], completes[i]);
					String s2 = convertValue(item2, attributes[i], o2, limits[i], completes[i]);
					if (s1 == null) {
						if (s2 != null) {
							return 1;
						}
					} else if (s2 == null) {
						return -1;
					}
					int result = s1.compareTo(s2);
					if (result != 0) {
						return result;
					}
				}
			}
		}
		return 0;
	}
	
	public String getType() {
		return type;
	}

	public boolean isAllowTabulations() {
		return allowTabulations;
	}

	public void setType(String type) {
		this.type = type;
	}

	public void setAllowTabulations(boolean allowTabulations) {
		this.allowTabulations = allowTabulations;
	}

	/**
	 * Determine if references lines (codes this dots, like into "status.code") are allowed into the format string.
	 * @param allowReferences the flag value to set
	 */
	public void setAllowReferences(boolean allowReferences) {
		this.allowReferences = allowReferences;
	}

	/**
	 * @return the allowReferences
	 */
	public boolean isAllowReferences() {
		return allowReferences;
	}

}
