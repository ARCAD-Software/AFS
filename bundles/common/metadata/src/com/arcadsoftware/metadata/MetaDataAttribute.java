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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.ParseException;

import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IIdentifiedBean;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.osgi.ISODateFormater;

public class MetaDataAttribute extends Element {

	private static final long serialVersionUID = -2975033480251629047L;

	public static final String TYPE_TRANSLATE = "translate"; //$NON-NLS-1$
	public static final String TYPE_INT = "int"; //$NON-NLS-1$
	public static final String TYPE_INTEGER = "integer"; //$NON-NLS-1$
	public static final String TYPE_RANGE = "range"; //$NON-NLS-1$
	public static final String TYPE_FLOAT = "float"; //$NON-NLS-1$
	public static final String TYPE_DATE = "date"; //$NON-NLS-1$
	public static final String TYPE_ICON = "icon"; //$NON-NLS-1$
	public static final String TYPE_STRING = "string"; //$NON-NLS-1$
	public static final String TYPE_BOOLEAN = "boolean"; //$NON-NLS-1$
	public static final String TYPE_REALBOOLEAN = "realboolean"; //$NON-NLS-1$
	public static final String TYPE_URL = "url"; //$NON-NLS-1$
	public static final String TYPE_EMAIL = "email"; //$NON-NLS-1$
	public static final String TYPE_BIGINTEGER = "biginteger"; //$NON-NLS-1$
	public static final String TYPE_LONG = "long"; //$NON-NLS-1$
	// Note: If you have to declare another "atomic" type in this list.
	// Please consider to and it to the "isSimpleType" method. Doing is will 
	// prevent any MetadataEntity to use this name as a type.
	
	private int length;
	private int precision;
	private Boolean listable;
	private Boolean mandatory;
	private ISearchCriteria update;
	private ISearchCriteria read;
	
	public MetaDataAttribute(MetaDataEntity parent, String type) {
		super(parent, null, type);
	}

	public MetaDataAttribute(MetaDataEntity parent) {
		super(parent);
	}

	public MetaDataAttribute(MetaDataAttribute attribute, MetaDataEntity parent) {
		super(attribute, parent);
		if (attribute.length != 0) {
			length = attribute.length;
		}
		if (attribute.precision != 0) {
			precision = attribute.precision;
		}
		if (attribute.listable != null) {
			listable = attribute.listable;
		}
		if (attribute.mandatory != null) {
			mandatory = attribute.mandatory;
		}
		if (attribute.read != null) {
			read = attribute.read;
		}
		if (attribute.update != null) {
			update = attribute.update;
		}
	}

	public MetaDataAttribute(MetaDataAttribute attribute) {
		this(attribute,attribute.getParent());
	}

	/**
	 * Get the modification right associated to this attribute.
	 * 
	 * <p>
	 * If this attribute does not define an update right then this right can be deduced from
	 * the parent entity update right.
	 *  
	 * @param inherited if true the right is deduced from referenced entities.
	 * @return A criteria or null if not right are required (if inherited is false).
	 * @see MetaDataEntity#getRightUpdate()
	 */
	public ISearchCriteria getRightUpdate(boolean inherited) {
		if (update != null) {
			return update;
		}
		if (inherited) {
			return getParent().getRightUpdate();
		}
		return null;
	}

	public ISearchCriteria getRightRead(boolean inherited) {
		if (read != null) {
			return read;
		}
		if (inherited) {
			return getParent().getRightRead();
		} 
		return null;
	}

	public void setUpdate(ISearchCriteria write) {
		this.update = write;
	}

	public void setRead(ISearchCriteria read) {
		this.read = read;
	}

	/**
	 * For "string" attributes determine the maximal length of the value.
	 * <p>
	 * For "integer" attributes non null value determine if the values must be a positive nor a negative one.
	 * <p>
	 * etc.
	 * 
	 * @return zero if non limited.
	 */
	public int getLength() {
		return length;
	}

	/**
	 * Define the length of this attribute.
	 * 
	 * @param length
	 */
	public void setLength(int length) {
		this.length = length;
	}

	/**
	 * Define if this attribute will be listed from server list results.
	 * 
	 * @param listable
	 */
	public void setListable(boolean listable) {
		this.listable = listable;
	}

	/**
	 * @return True if this attribute will be listed by default for the server BeanMap lists.
	 */
	public boolean isListable() {
		return (listable != null) && listable;
	}

	public void setMandatory(boolean mandatory) {
		this.mandatory = mandatory;
	}

	/**
	 * @return True if this attribute must be not empty (i.e. not null ou equal to an empty string) when edited.
	 */
	public boolean isMandatory() {
		return (mandatory != null) && mandatory;
	}

	public Boolean getListable() {
		return listable;
	}

	public Boolean getMandatory() {
		return mandatory;
	}

	/**
	 * @param precision the precision to set
	 */
	public void setPrecision(int precision) {
		this.precision = precision;
	}

	/**
	 * @return the precision
	 */
	public int getPrecision() {
		return precision;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return new MetaDataAttribute(this);
	}

	public boolean isSimpleType() {
		String type = getType();
		return TYPE_BOOLEAN.equals(type) ||
				TYPE_DATE.equals(type) ||
				TYPE_FLOAT.equals(type) ||
				TYPE_ICON.equals(type) ||
				TYPE_INT.equals(type) ||
				TYPE_INTEGER.equals(type) ||
				TYPE_BIGINTEGER.equals(type) ||
				TYPE_LONG.equals(type) ||
				TYPE_RANGE.equals(type) ||
				TYPE_STRING.equals(type) ||
				TYPE_TRANSLATE.equals(type) ||
				TYPE_URL.equals(type) ||
				TYPE_EMAIL.equals(type);
	}
	
	/**
	 * <p>This is more efficient to directly get the type entity, if null then this attribute can be considered as an non reference attribute (primary type).
	 * @return
	 * @see #isSimpleType()
	 */
	public boolean isReference() {
		return getRefEntity() != null;
	}
	
	/**
	 * 
	 * @return the reference entity if it exist, null if this attribute is a primary value attribute.
	 */
	public MetaDataEntity getRefEntity() {
		return getParent().getEntity(getType());
	}

	/**
	 * Translatable attribute are not stored into a database. 
	 * 
	 * <p>
	 * These attrributes are "virtual" they are translated on demands according to given properties files. 
	 * @return true if this attribute type is "translate".
	 */
	public boolean isTranslatable() {
		return TYPE_TRANSLATE.equals(getType());
	}

	/**
	 * Translate a translatable attribute and store the translated value into the given BeanMap. 
	 * @param bm
	 * @param language
	 */
	public void translate(BeanMap bm, Language language) {
		String code = bm.getString(IIdentifiedBean.KEY_CODE);
		if ((code == null) || (code.length() == 0)) {
			code = Integer.toString(bm.getId());
		}
		// Règle de traduction {type}.{attCode}.{code ou id de la ligne}
		bm.put(getCode(), Activator.getInstance().translate(Activator.TRANLATEDOMAIN_DATA, getParent().getType() + '.' + getCode() + '.' + code, language));
	}

	/**
	 * Determine if this attribute is a reference to an entity that is define into the same domain.
	 * 
	 * @return
	 */
	public boolean isLocalReference() {
		MetaDataEntity ref = getRefEntity();
		return (ref != null) && //
				(getParent() != null) && //
						(getParent().getMapper() != null) && //
						getParent().getMapper().sameDomain(ref);
	}

	public void setVisible(boolean value) {
		getMetadata().put(MetaDataEntity.METADATA_VISIBLE, value);
	}

	public boolean isVisible() {
		return getMetadata().getBoolean(MetaDataEntity.METADATA_VISIBLE);
	}

	public void setColRank(int value) {
		getMetadata().put(MetaDataEntity.METADATA_COLRANK, value);
	}
	
	public int getColRank() {
		return getMetadata().getInt(MetaDataEntity.METADATA_COLRANK);
	}

	public void setColSize(int value) {
		getMetadata().put(MetaDataEntity.METADATA_COLSIZE, value);
	}

	public int getColSize() {
		return getMetadata().getInt(MetaDataEntity.METADATA_COLSIZE);
	}

	/**
	 * @return true is this attribute is not HIDDEN, it is returned to the Client by the web-services.
	 */
	public boolean isPublic() {
		return !getMetadata().getBoolean(MetaDataEntity.METADATA_HIDDEN);
	}

	/**
	 * @return true if the attribute type is "boolean".
	 */
	public boolean isBoolean() {
		return TYPE_BOOLEAN.equals(getType());
	}

	/**
	 * @return true if the attribute type is "date"
	 */
	public boolean isDate() {
		return TYPE_DATE.equals(getType());
	}

	/**
	 * @return true if this attribute is a numeric attribute (numeric are "integer", "float" and "boolean").
	 */
	public boolean isNumeric() {
		return TYPE_INT.equalsIgnoreCase(getType()) || //
				TYPE_INTEGER.equalsIgnoreCase(getType()) || //
				TYPE_FLOAT.equalsIgnoreCase(getType()) || //
				TYPE_BIGINTEGER.equalsIgnoreCase(getType()) || //
				TYPE_LONG.equalsIgnoreCase(getType()) || //
				TYPE_BOOLEAN.equalsIgnoreCase(getType());
	}

	/**
	 * @return true if this attribute is a String based attribute (like "String" "email", "url", etc...).
	 */
	public boolean isString() {
		return TYPE_STRING.equalsIgnoreCase(getType()) || //
				TYPE_EMAIL.equalsIgnoreCase(getType()) || //
				TYPE_URL.equalsIgnoreCase(getType());
	}

	/**
	 * Translate the Attribute name to the given client user language.
	 * 
	 * <p>
	 * Try to translate the name of this attribute. If this translation is undefined then return the
	 * current value of the name or the code of this attribute.
	 * 
	 * <p>
	 * This method is not efficient to translate a whole MetaDataEntity. In most case it is useless
	 * on client side, because this is the MetadataEntity web-service responsibility to serve translated object.
	 * 
	 * @param language
	 * @return a non null String representation of this attribute.
	 * @see MetaDataEntity#clone(Language)
	 */
	public String getName(Language language) {
		String n = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, getParent().getType().replace('/', '.') + '.' + getCode(), language);
		if (n == null) {
			if (getName() != null) {
				return getName();
			}
			return getCode();
		}
		return n;
	}
	
	/**
	 * Convert any string representation of this attribute value into a more specific Object, that may be suitable for a database mapping.
	 * 
	 * @param value
	 * @return a converted value, may be null even if the value parameter is not null.
	 */
	public Object convertValue(String value) {
		if (MetaDataAttribute.TYPE_BOOLEAN.equals(getType())) {
			if ("true".equalsIgnoreCase(value) || //$NON-NLS-1$
					"yes".equalsIgnoreCase(value) || //$NON-NLS-1$
					"1".equalsIgnoreCase(value)) { //$NON-NLS-1$
				return Integer.valueOf(1);
			}
			return Integer.valueOf(0);
		}
		if (MetaDataAttribute.TYPE_REALBOOLEAN.equals(getType())) {
			if ("true".equalsIgnoreCase(value) || //$NON-NLS-1$
					"yes".equalsIgnoreCase(value) || //$NON-NLS-1$
					"1".equalsIgnoreCase(value)) { //$NON-NLS-1$
				return Boolean.TRUE;
			}
			return Boolean.FALSE;
		}
		// As the original Form may come from a JSON object conversion "null" values should be supported...
		if (value == null) {
			return null;
		}
		if (MetaDataAttribute.TYPE_DATE.equals(getType())) {
			if (value.isEmpty()) {
				// Support null dates.
				return null;
			}
			if (ISODateFormater.mayIsoDate(value)) {
				try {
					return ISODateFormater.toDate(value);
				} catch (ParseException e) {}// nothing to do.
			}
		}
		if (MetaDataAttribute.TYPE_INTEGER.equals(getType()) ||
				MetaDataAttribute.TYPE_INT.equals(getType())) { 
			int i = 0;
			try {
				i = Integer.parseInt(value);
			} catch (NumberFormatException e) {}
			if ((length != 0) && (i > length)) {
				return Integer.valueOf(length);
			}
			return Integer.valueOf(i);
		}
		if (MetaDataAttribute.TYPE_LONG.equals(getType())) { 
			long l = 0;
			try {
				l = Long.parseLong(value);
			} catch (NumberFormatException e) {}
			if ((length != 0) && (l > length)) {
				return Long.valueOf(length);
			}
			return Long.valueOf(l);
		}
		if (MetaDataAttribute.TYPE_ICON.equals(getType())) { 
			int i = 0;
			try {
				i = Integer.parseInt(value);
			} catch (NumberFormatException e) {}
			if (i <= 0) {
				return null;
			}
			return Integer.valueOf(i);
		}
		if (MetaDataAttribute.TYPE_RANGE.equals(getType())) { 
			int i = 0;
			try {
				i = Integer.parseInt(value);
			} catch (NumberFormatException e) {}
			if (length > precision) {
				if (i > length) {
					return Integer.valueOf(length);
				}
				if (i < precision) {
					return Integer.valueOf(precision);
				}
			}
			return Integer.valueOf(i);
		}
		// Range and scale Float value
		if (MetaDataAttribute.TYPE_FLOAT.equals(getType())) {
			double f = 0;
			try {
				f = Double.parseDouble(value);
			} catch (NumberFormatException e) {}
			if ((length > 0) && (f > length)) {
				return Double.valueOf(length);
			}
			if (precision > 0) {
				f = new BigDecimal(f).setScale(precision, RoundingMode.HALF_UP).doubleValue();
			}
			return Double.valueOf(f);
		}
		if (MetaDataAttribute.TYPE_BIGINTEGER.equals(getType())) {
			try {
				return new BigInteger(value);
			} catch (NumberFormatException e) {
				return new BigInteger(new byte[] {0});
			}
		}
		// Truncate the strings.
		if (MetaDataAttribute.TYPE_STRING.equals(getType()) && (length > 0)) { 
			if (value.length() > length) {
				value = value.substring(0, length);
			}
			return value;
		}
		if (isReference()) {
			int ref = 0;
			try {
				ref = Integer.parseInt(value);
			} catch (NumberFormatException e) {}
			if (ref <= 0) {
				// Support null references.
				return null;
			}
			return Integer.valueOf(ref);
		}
		// misc. types...
		// Any attribute with Precision > 0 is an integer of type "Range"...
		// if length is lower than precision then "precision" is a lower limit.
		if (precision > 0) {
			int i = 0;
			try {
				i = Integer.parseInt(value);
			} catch (NumberFormatException e) {}
			if ((length > precision) && (i > length)) {
				return Integer.valueOf(length);
			}
			if (i < precision) {
				return Integer.valueOf(precision);
			}
			return Integer.valueOf(i);
		}
		// Any Attribute if Length > 0 (and precision = 0, see above) is assumed to be a string.
		if (length > 0) {
			if (value.length() > length) {
				value = value.substring(0, length);
			}
		}
		return value;
	}
}
