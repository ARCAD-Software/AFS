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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.beanmap.IDatedBean;
import com.arcadsoftware.beanmap.ITypedBean;
import com.arcadsoftware.metadata.criteria.AbstractSearchCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.CriteriaContextBasic;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This class define the structure of an entity that can be stored into a BeanMap. 
 * 
 * <p>
 * The EntityAttribute list define the possible contain of the BeanMap. 
 * The EntityLink are external link, regarding to the BeanMap, between this Entity and
 * another one.
 * 
 * <p>
 * Excepted theses list of sub-elements, the more important properties of this EntityStructure are:
 * 
 * <ul>
 * <li>The Type, a key that unically define this EntityStructure (In other words, the type
 *   define the primary key of this entity structure).
 * <li>The Read-only capability.
 * <li>The structure last modification date. Used for caching this object.
 * <li>The lockable capability that can ensure that the user that lock an element of this kind
 *   of type is the only one allowed to modify it.
 * <li>The ControlTest that are JavaScript programs used to verify the consistency of the data.
 * </ul>
 */
public class MetaDataEntity  implements Serializable, Cloneable, IDatedBean, ITypedBean, Comparable<MetaDataEntity> {

	private static final long serialVersionUID = -2129638450698377277L;

	/**
	 * This tag indicate that the entity must fire read event when a item is read (GET from
	 * <code>/data/{type}/{id}</code>). Read events are consuming the, by default, they are not fired.
	 */
	public static final String METADATA_EVENTONREAD = "eventOnRead"; //$NON-NLS-1$

	/**
	 * This tag indicate that the entity must fire selection event when some item are selected (GET from
	 * <code>/data/{type}</code>). Selection events are consuming the, by default, they are not fired.
	 */
	public static final String METADATA_EVENTONSELECTION = "eventOnSelection"; //$NON-NLS-1$

	/**
	 * Present into a Link Metadata this tag indicate that this link is a "reverse link". The value of the tag is an
	 * attribute code taken from the target entity that reference the origin one. The association is automatically built
	 * from the references from the target entity.
	 * 
	 * <p>
	 * Used into an entity this tag automatically generate all the reverse link that can be found starting from the
	 * current entity. Each time an entity reference the "reverselink" entity then a reverse link is declared into this
	 * one pointing to the referencing.
	 * 
	 * <p>
	 * Used into an attribute this tag automatically generate the reverse link corresponding to this attribute into the
	 * referenced entity.
	 */
	public static final String METADATA_REVERSELINK = "reverseLink"; //$NON-NLS-1$

	/**
	 * This tag indicate that the entity will automatically declare links for any link that target its. For instance, if
	 * an entity A contain the "autolink" tag, then if a link is declared into an entity B targeting A then a new link
	 * is automatically declared into A pointing to entity B.
	 * 
	 * <p>
	 * The "autolink" tag, used at the link level, indicate that this link is the result of a inversion of a standard
	 * link.
	 */
	public static final String METADATA_AUTOLINK = "autoLink"; //$NON-NLS-1$

	/**
	 * This tag indicate that any link creation or suppression must update the target and the source entity Update and Modification User values.
	 * 
	 * <p>
	 * The "pushUpdate" tag is only used at the link level. For "reverseLink" it just imply that the modification of the
	 * attribute value is pushed to the target one. 
	 */
	public static final String METADATA_PUSHUPDATELINK = "pushUpdate"; //$NON-NLS-1$
	
	/**
	 * This tag is used by client and GUI processes to visually order Attributes of an entity.
	 * 
	 * <p>
	 * The "colrank" tag is used at the attribute level.
	 */
	public static final String METADATA_COLRANK = "colrank"; //$NON-NLS-1$

	/**
	 * This tag is used by client and GUI processes to visually order Attributes of an entity.
	 * 
	 * <p>
	 * The "colsize" tag is used at the attribute level.
	 */
	public static final String METADATA_COLSIZE = "colsize"; //$NON-NLS-1$

	/**
	 * This tag is used to visually hide some Attributes into the GUI.
	 * 
	 * <p>
	 * The "visible" tag is used at the attribute level.
	 */
	public static final String METADATA_VISIBLE = "visible"; //$NON-NLS-1$

	/**
	 * This tag is used to determine if the modification of a given attribute or link is journalized.
	 * 
	 * <p>
	 * The "logs" tag is used at the attribute level.
	 */
	public static final String METADATA_LOGS = "logs"; //$NON-NLS-1$

	/**
	 * This tag is used to determine if the date of the last modification of an item is exploitable.
	 * 
	 * <p>
	 * The "updatable" tag is used at the entity level.
	 */
	public static final String METADATA_UPDATABLE = "updatable"; //$NON-NLS-1$

	/**
	 * Used within an entity this tag define the binary file transfert category associated
	 * with this Entity.
	 * 
	 *  <p>
	 *  Any "binary" entity data will be associated with zero or many binary file managed with
	 *  the centralized "binary file transfer service". The binary tag value define the
	 *  category where theses files will be recorded. By convention this folder
	 *  should be divided into to sub-folder : &lt;solution&gt;/&lt;type&gt;
	 *  
	 */
	public static final String METADATA_BINARY = "binary"; //$NON-NLS-1$

	/**
	 * Used to define the "code" attribute of an Entity. The Code attribute, is a "functionnal key"
	 * that may be used to access the data when the ID is unknown.
	 * 
	 * <p>
	 * Default value of this metadata is the string "code". Setting it to a empty string, or if 
	 * no "code" attribute is define will disable this functionality.
	 * 
	 * <p>
	 * Note that this attribute should use the "unique" metadata to ensure that its value is unique.
	 * 
	 * @see #METADATA_UNIQUE
	 */
	public static final String METADATA_CODE = "attributeCode"; //$NON-NLS-1$
	
	/**
	 * TODO Implement the lazzy serving of data...
	 */
	public static final String METADATA_LAZZY = "lazzy"; //$NON-NLS-1$

	/**
	 * Used by attributes to indicate that a constraint may exist on their value.
	 */
	public static final String METADATA_UNIQUE = "unique"; //$NON-NLS-1$

	/**
	 * Used by attributes to indicate that the value should be encoded during process.
	 * <p>
	 * Accepted values are (case insensitive):
	 * <ul>
	 * <li>All/Any/True = encoded during whole process.
	 * <li>Storage/Database/Mapper = encoded when stored in database.
	 * <li>Web/Transfer/HTTP/REST = (Not Yet implemented !).
	 * <li>None/False = never encoded.
	 * </ul>
	 */
	public static final String METADATA_CRYPT = "crypt"; //$NON-NLS-1$

	/**
	 * The associated attribute of link is never sent back to the client through web-services or 
	 * any other remote communication protocol.
	 */
	public static final String METADATA_HIDDEN = "hidden"; //$NON-NLS-1$

	/**
	 * The associated a link thisd link is not considered as a recursive one even if the targeted entity is the same one.
	 */
	public static final String METADATA_IGNORERECURSIVITY = "norecursive"; //$NON-NLS-1$

	/**
	 * use in link which are composed with a chain of other links.
	 */
	public static final String METADATA_COMBOLINK = "combo"; //$NON-NLS-1$

	// Some other MetaDate can be defined by other bundles.
	// You could have a look to the MapperSQLService.

	/**
	 * This method allow any bundle to get a Entity.
	 * 
	 * <p>
	 * This entity object should not be stored in memory since it can be updated or deleted from the registry that own
	 * it.
	 * 
	 * <p>
	 * <b>Note that this method is only usable on server (where there is no multiple sources of entities).
	 * <b> To be able to load an entity on the client side use an already loaded entity and the getEntity(String type) method.
	 * 
	 * @param type
	 * @return
	 * @see #getEntity(String)
	 */
	public static MetaDataEntity loadEntity(String type) {
		if ((type == null) || (type.length() == 0)) {
			return null;
		}
		Activator activator = Activator.getInstance();
		if (activator == null) {
			return null;
		}
		return activator.getEntity(type);
	}

	/**
	 * Returns the binary category of a given type, as described on {@link #METADATA_BINARY}.
	 * 
	 * @param entityType
	 * @return the type's binary category. 
	 */
	public static String getEntityBinary(String entityType) {
		MetaDataEntity entity = loadEntity(entityType);
		if (entity != null) {
			return entity.getMetadata().getString(METADATA_BINARY);
		}
		return null;
	}
	
	/**
	 * This method allow any bundle to get a list of Entities.
	 * 
	 * <p>
	 * Return the list of declared entities for the given domain name.
	 * 
	 * @param domain
	 * @return
	 */
	public static List<MetaDataEntity> loadEntities(String domain) {
		Activator activator = Activator.getInstance();
		if (activator == null) {
			return new ArrayList<MetaDataEntity>();
		}
		return activator.getEntities(domain);
	}
	
	/**
	 * This method allow any bundle to get a list of Entities.
	 * 
	 * @return
	 */
	public static List<MetaDataEntity> loadEntities() {
		Activator activator = Activator.getInstance();
		if (activator == null) {
			return new ArrayList<MetaDataEntity>();
		}
		return activator.getEntities();
	}

	private static XmlCriteriaStream xscriteria = new XmlCriteriaStream();
	
	private static ISearchCriteria getCriteria(String criteria) {
		if ((criteria == null) || (criteria.length() == 0)) {
			return null;
		}
		return (ISearchCriteria) xscriteria.fromXML(criteria);
	}
	
	private String type;
	private String name;
	private String description;
	private BeanMap metadata;
	private String domain;
	private int version;
	private ISearchCriteria create;
	private ISearchCriteria update;
	private ISearchCriteria read;
	private ISearchCriteria list;
	private ISearchCriteria delete;
	protected Boolean readonly; // this value need to be nullable
	private Date date;
	protected Boolean lockable; // this value need to be nullable
	private String groupType;
	private HashMap<String, MetaDataAttribute> attributes = new HashMap<String, MetaDataAttribute>();
	private HashMap<String, MetaDataLink> links = new HashMap<String, MetaDataLink>();
	private HashMap<String, MetaDataTest> tests = new HashMap<String, MetaDataTest>();
	private transient IMapperService mapper;
	private transient IEntityRegistry registry;

	/**
	 * Create a new empty entity.
	 * 
	 * @param type
	 */
	public MetaDataEntity(String type) {
		super();
		this.type = type;
		metadata = new BeanMap();
		setDate(new Date());
		version = 1;
	}

	/**
	 * Create a new empty entity.
	 * 
	 * @param version
	 */
	public MetaDataEntity(int version) {
		this((String) null);
		this.version = version;
	}

	/**
	 * Create a new empty entity.
	 * 
	 * @param type
	 * @param version
	 */
	public MetaDataEntity(String type, int version) {
		this(type);
		this.version = version;
	}

	/**
	 * Create a new empty entity.
	 * 
	 * @param id
	 * @param type
	 * @param domainName
	 */
	public MetaDataEntity(String type, String domainName) {
		this(type);
		domain = domainName;
	}

	/**
	 * Create a new empty entity from the given registry.
	 * 
	 * @param id
	 * @param type
	 * @param registry
	 */
	public MetaDataEntity(String type, IEntityRegistry registry) {
		this(type);
		this.registry = registry;
	}

	/**
	 * Create a new empty entity from given resigstry and associated to given the mapper.
	 * 
	 * @param id
	 * @param type
	 * @param mapper
	 * @param registry
	 */
	public MetaDataEntity(String type, IMapperService mapper, IEntityRegistry registry) {
		this(type, registry);
		this.mapper = mapper;
	}

	/**
	 * Create a new empty entity from the given registry.
	 * 
	 * @param id
	 * @param registry
	 */
	public MetaDataEntity(IEntityRegistry registry) {
		super();
		this.registry = registry;
		metadata = new BeanMap();
	}

	/**
	 * Duplicate an Entity.
	 * @param entity
	 */
	public MetaDataEntity(MetaDataEntity entity) {
		this(entity.getType());
		inject(entity, true);
	}
	
	/**
	 * Create a new translated instance of this entity.
	 * 
	 * @param entity
	 * @param language
	 */
	public MetaDataEntity(MetaDataEntity entity, Language language) {
		this(entity.getType());
		inject(entity, true);
		translate(language);
	}

	private void translate(Language language) {
		String typecode = type.replace('/', '.');
		String name = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, typecode, language);
		if (typecode.equals(name)) {
			if (getName() == null) {
				setName(type);
			}
		} else {
			setName(name);
		}
		typecode += '.';
		String descCode = typecode + 'd';
		String desc = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, descCode, language);
		if (!descCode.equals(desc)) {
			setDescription(desc);
		}
		for(MetaDataAttribute a:attributes.values()) {
			String code = typecode + a.getCode();
			name = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, code, language);
			if (code.equals(name)) {
				if (a.getName() == null) {
					a.setName(a.getCode());
				}
			} else {
				a.setName(name);
			}
			code = code + ".d"; //$NON-NLS-1$
			desc = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, code, language);
			if (!code.equals(desc)) {
				a.setDescription(desc);
			}
		}
		for(MetaDataLink l:links.values()) {
			String code = typecode + "link." + l.getCode(); //$NON-NLS-1$
			name = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, code, language);
			if (code.equals(name)) {
				if (l.getName() == null) {
					l.setName(l.getCode());
				}
			} else {
				l.setName(name);
			}
			code = code + ".d"; //$NON-NLS-1$
			desc = Activator.getInstance().translate(Activator.TRANLATEDOMAIN_ENTITY, code, language);
			if (!code.equals(desc)) {
				l.setDescription(desc);
			}
		}
	}

	/**
	 * Used when a sub-element change of code.
	 * This modification need to be reported into the entity indexed tables.
	 * 
	 * @param oldCode The previous code.
	 * @param element The corresponding element (with its new code).
	 */
	protected void reIndexElement(String oldCode, Object element) {
		if (element instanceof MetaDataAttribute) {
			attributes.remove(oldCode);
			attributes.put(((MetaDataAttribute) element).getCode(), (MetaDataAttribute) element);
		} else if (element instanceof MetaDataLink) {
			links.remove(oldCode);
			links.put(((MetaDataLink) element).getCode(), (MetaDataLink) element);
		} else if (element instanceof MetaDataTest) {
			tests.remove(oldCode);
			tests.put(((MetaDataTest) element).getCode(), (MetaDataTest) element);
		}
	}

	/**
	 * Get another entity from the same Registry.
	 * 
	 * @param type
	 * @return
	 * @see #loadEntity(String)
	 */
	public MetaDataEntity getEntity(String type) {
		if (registry != null) {
			MetaDataEntity entity = registry.getEntity(type);
			if (entity != null) {
				return entity;
			}
		}
		Activator activator = Activator.getInstance();
		if (activator == null) {
			return null;
		}
		return activator.getEntity(type);
	}


	@Override
	public MetaDataEntity clone() {
		return new MetaDataEntity(this);
	}

	/**
	 * This method create a copy of the original entity, with the following modifications :
	 * 
	 * <ul>
	 * <li> The translatable properties are translated in the given language or in english.
	 * <li> All the metadata relative to the server part are removed.
	 * </ul>
	 *  
	 * @param language the desired Language.
	 * @return the cloned and translated entity declaration.
	 */
	public MetaDataEntity clone(Language language) {
		MetaDataEntity result =  new MetaDataEntity(this, language);
		// TODO We need a registry of "server side metadata" !
		result.metadata.remove("table"); //$NON-NLS-1$
		result.metadata.remove("deleteCol"); //$NON-NLS-1$
		result.metadata.remove("colPrefix"); //$NON-NLS-1$
		result.metadata.remove("idCol"); //$NON-NLS-1$
		result.metadata.remove("updateCol"); //$NON-NLS-1$
		result.metadata.remove("lockCol"); //$NON-NLS-1$
		result.metadata.remove("lockDateCol"); //$NON-NLS-1$
		result.metadata.remove("groupTable"); //$NON-NLS-1$
		result.metadata.remove("groupMinCol"); //$NON-NLS-1$
		result.metadata.remove("groupMaxCol"); //$NON-NLS-1$
		result.metadata.remove(METADATA_EVENTONREAD);
		result.metadata.remove(METADATA_EVENTONSELECTION);
		result.metadata.remove("ldap"); //$NON-NLS-1$
		result.metadata.remove("logDelete"); //$NON-NLS-1$
		result.metadata.remove("logUpdate"); //$NON-NLS-1$
		result.metadata.remove("logCreate"); //$NON-NLS-1$
		result.metadata.remove("logUnlink"); //$NON-NLS-1$
		result.metadata.remove("logLink"); //$NON-NLS-1$
		result.metadata.remove("replace"); //$NON-NLS-1$
		for (MetaDataAttribute a: result.attributes.values()) {
			a.getMetadata().remove("col"); //$NON-NLS-1$
			a.getMetadata().remove("ldap"); //$NON-NLS-1$
		}
		for (MetaDataLink l: result.links.values()) {
			l.getMetadata().remove("table"); //$NON-NLS-1$
			l.getMetadata().remove("sourceCol"); //$NON-NLS-1$
			l.getMetadata().remove("destCol"); //$NON-NLS-1$
			l.getMetadata().remove("deleteCol"); //$NON-NLS-1$
			l.getMetadata().remove("logUnlink"); //$NON-NLS-1$
			l.getMetadata().remove("logLink"); //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * Indicate if this entity is readonly (can not be modified onto the server from direct user action).
	 * @return
	 */
	public boolean isReadOnly() {
		return (readonly != null) && readonly;
	}

	/**
	 * Flag this entity as readonly, prevent from user modifications.
	 * @param readonly
	 */
	public void setReadOnly(boolean readonly) {
		this.readonly = readonly;
	}

	/**
	 * Change the entity Type (use with caution)
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}
	
	/**
	 * @return the list of the Control Test (JavaScript).
	 */
	public HashMap<String,MetaDataTest> getTests() {
		return tests;
	}

	/**
	 * Get the Test associated to the given code.
	 * 
	 * @param code
	 * @return
	 */
	public MetaDataTest getTest(String code) {
		return tests.get(code);
	}

	/**
	 * Set the list of Controls Tests.
	 * @param tests
	 */
	public void setTests(HashMap<String,MetaDataTest> tests) {
		this.tests = tests;
	}

	/**
	 * The lockable capability that can ensure that the user that lock an element of this kind
	 * of type is the only one allowed to modify it.
	 * 
	 * @return
	 */
	public boolean isLockable() {
		return (lockable != null) && lockable;
	}

	/**
	 * The lockable capability that can ensure that the user that lock an element of this kind
	 * of type is the only one allowed to modify it.
	 * 
	 * @param lockable
	 */
	public void setLockable(boolean lockable) {
		this.lockable = lockable;
	}
	
	/**
	 * @return true if this entity reprensent a Group entity.
	 */
	public boolean isGroup() {
		return (groupType != null) && (groupType.length() > 0);
	}

	/**
	 * The type of the group items. 
	 * 
	 * <p>This type is different from the group type itself.
	 *   
	 * @return null if this entity is not a group.
	 */
	public String getGroupType() {
		return groupType;
	}

	/**
	 * Define the type of element of this group entity.
	 * 
	 * <p>Null if this entity is not a group.
	 * 
	 * @param groupType
	 */
	public void setGroupType(String groupType) {
		this.groupType = groupType;
	}

	/**
	 * Get the attributes codes separated by spaces.
	 * 
	 * @return The list of all the attributes codes.
	 */
	public String getLinkCodes() {
		StringBuilder sb = new StringBuilder();
		boolean spaces = false;
		for (MetaDataLink lk: links.values()) {
			if (spaces) {
				sb.append(' ');
			} else {
				spaces = true;
			}
			sb.append(lk.getCode());
		}
		return sb.toString();
	}

	/**
	 * Return the corresponding link.
	 * 
	 * @param code
	 *            the unique link code.
	 * @return
	 */
	public MetaDataLink getLink(String code) {
		return links.get(code);
	}

	/**
	 * Get the whole links hash table.
	 * @return
	 */
	public HashMap<String, MetaDataLink> getLinks() {
		return links;
	}

	/**
	 * Set the whole links hash table.
	 * @param links
	 */
	public void setLinks(HashMap<String, MetaDataLink> links) {
		this.links = links;
	}

	/**
	 * Return the corresponding attribute.
	 * 
	 * @param code
	 *            The unique attribute code.
	 * @return
	 */
	public MetaDataAttribute getAttribute(String code) {
		return attributes.get(code);
	}
	
	/**
	 * Return a element from this Entity (e.g. an Attribute or a Link).
	 * @param code
	 * @return
	 */
	public Element getElement(String code) {
		MetaDataAttribute a = attributes.get(code);
		if (a != null) {
			return a;
		}
		return links.get(code);
	}

	/**
	 * Return the reference line stating from this entity.
	 * 
	 * <p>
	 * A reference line is coded this attributes codes chained with dots.
	 * 
	 * <p>
	 * For ordering purposes the attributes code may be "marked" with a "!" prefix. This indicate a descending
	 * order.
	 * 
	 * @param code
	 *            the code of the reference line.
	 * @return null if the entity is not correctly initialized or the code bring to a inexistant entity.
	 */
	public ReferenceLine getAttributeLine(String code) {
		// TODO Utiliser un cache interne à l'entité...
		if ((code == null) || (code.length() == 0)) {
			return null;
		}
		boolean flag = code.charAt(0) == '!';
		if (flag) {
			code = code.substring(1);
		}
		String[] codes = code.split("\\."); //$NON-NLS-1$
		ReferenceLine list = new ReferenceLine(code, codes.length, flag);
		MetaDataEntity entity = this;
		for (String c : codes) {
			MetaDataAttribute att = entity.getAttribute(c);
			if (att == null) {
				return null;
			}
			list.add(att);
			entity = getEntity(att.getType());
			if (entity == null) {
				return list;
			}
		}
		return list;
	}

	/**
	 * Return the reference lines stating from this entity.
	 * 
	 * <p>
	 * A reference line is coded with attributes codes chained with dots.
	 * 
	 * <p>
	 * For ordering purposes the attributes code may be "marked" with a "!" prefix. This indicate a descending
	 * order.
	 * 
	 * @param code
	 *            the code of the reference lines.
	 * @return never return null, but the list may be empty if the codes does not matches.
	 */
	public List<ReferenceLine> getAttributeLines(String... codes) {
		ArrayList<ReferenceLine> result = new ArrayList<ReferenceLine>();
		for(String code: codes) {
			ReferenceLine ref = getReferenceLine(code);
			if (ref != null) {
				result.add(ref);
			}
		}
		return result;
	}

	/**
	 * Return the reference lines stating from this entity of attributes and link that are not "hidden".
	 * 
	 * <p>
	 * A reference line is coded with attributes codes chained with dots.
	 * 
	 * <p>
	 * For ordering purposes the attributes code may be "marked" with a "!" prefix. This indicate a descending
	 * order.
	 * 
	 * @param code
	 *            the code of the reference lines.
	 * @return never return null, but the list may be empty if the codes does not matches.
	 */
	public List<ReferenceLine> getPublicAttributeLines(String... codes) {
		ArrayList<ReferenceLine> result = new ArrayList<ReferenceLine>();
		for(String code: codes) {
			ReferenceLine ref = getReferenceLine(code);
			if ((ref != null) && !ref.isHidden()) {
				result.add(ref);
			}
		}
		return result;
	}

	/**
	 * Return the list of codes corresponding to the given prefix.
	 * 
	 * <p>
	 * Support references even with links.
	 * 
	 * @param codePrefix
	 * @return
	 */
	public List<String> getCompleteCode(String codePrefix) {
		if ((codePrefix == null) || (codePrefix.length() == 0)) {
			return getPrefixedCodes("",""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		String[] codes = codePrefix.split("\\."); //$NON-NLS-1$
		if ((codes == null) || (codes.length == 0)) {
			return getPrefixedCodes("", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		int end = codes.length -1;
		String endCode = codes[end];
		if (codePrefix.charAt(codePrefix.length() -1) == '.') {
			end++;
			endCode = ""; //$NON-NLS-1$
		}
		StringBuilder prefix = new StringBuilder();
		MetaDataEntity entity = this;
		for(int i = 0; i < end;i++) {
			String c = codes[i];
			if ((c == null) || (c.length() == 0)) {
				continue;
			}
			MetaDataAttribute att = entity.getAttribute(c);
			if (att != null)  {
				entity = att.getRefEntity();
			} else {
				MetaDataLink link = entity.getLink(c);
				if (link == null) {
					return null;
				}
				entity = link.getRefEntity();
			}
			if (entity == null) {
				ArrayList<String> result = new ArrayList<String>();
				prefix.append(c);
				result.add(prefix.toString());
				return result;
			}
			prefix.append(c);
			prefix.append('.');
		}
		return entity.getPrefixedCodes(prefix.toString(), endCode);
	}
	
	/**
	 * Get the code starting with the given text, and prefixed by prefix.
	 *  
	 * @param prefix
	 * @param startWith
	 * @return a non null list of prefixed codes.
	 * @see #getCompleteCode(String)
	 */
	protected List<String> getPrefixedCodes(String prefix, String startWith) {
		ArrayList<String> result = new ArrayList<String>(attributes.size());
		for(MetaDataAttribute attribute : attributes.values()) {
			String code = attribute.getCode();
			if (code.startsWith(startWith)) {
				result.add(prefix + code);
			}
		}
		for(MetaDataLink link : links.values()) {
			String code = link.getCode();
			if (code.startsWith(startWith)) {
				result.add(prefix + code);
			}
		}
		return result;
	}

	/**
	 * Return the reference line stating from this entity.
	 * 
	 * <p>
	 * A reference line is coded this attributes or link codes chained with dots.
	 * 
	 * @param code
	 *            the code of the reference line.
	 * @return null if the entity is not correctly initialized or the code bring to a inexistant entity.
	 */
	public ReferenceLine getReferenceLine(String code) {
		// TODO Use an internal cache of reference lines...
		if ((code == null) || (code.length() == 0)) {
			return null;
		}
		String[] codes = code.split("\\."); //$NON-NLS-1$
		ReferenceLine list = new ReferenceLine(code, codes.length);
		MetaDataEntity entity = this;
		int l = 0;
		for (String c : codes) {
			if (entity == null) {
				return null;
			}
			Element e = entity.getAttribute(c);
			if (e == null) {
				e = entity.getLink(c);
				if (e == null) {
					return null;
				}
				list.add(e);
				list.setIsLink(true);
				l++;
				if ((l > 1) && !(list.getLast() instanceof MetaDataLink)) {
					list.setIsMultiLink(true);
				}
				entity = getEntity(e.getType());
			} else if (((MetaDataAttribute) e).isSimpleType()) {
				list.add(e);
				break;
			} else {
				list.add(e);
				entity = getEntity(e.getType());
			}
		}
		if (list.isEmpty()) {
			return null;
		}
		return list;
	}

	/**
	 * Build the list of reference lines associated to the given codes, the codes are composed by attributes or link
	 * codes chained by dots, Each codes are separated with spaces.
	 * 
	 * <p>
	 * Incoherent or inexistent codes are removed from the list. Then the result list can be empty.
	 * 
	 * @param codes
	 * @return the references lines, or an empty list if none are found. Never return null.
	 */
	public List<ReferenceLine> getAttributeLines(String codes) {
		if ((codes == null) || (codes.length() == 0)) {
			return new ArrayList<ReferenceLine>();
		}
		String[] codestab = codes.split(" "); //$NON-NLS-1$
		if ((codestab == null) || (codestab.length == 0)) { // blindage
			return new ArrayList<ReferenceLine>();
		}
		List<ReferenceLine> result = new ArrayList<ReferenceLine>(codestab.length);
		for (String code : codestab) {
			ReferenceLine rl = getAttributeLine(code);
			if (rl != null) {
				result.add(rl);
			}
		}
		return result;
	}

	/**
	 * Build the list of reference lines associated to the given codes, the codes are composed by attributes or link
	 * codes chained by dots, Each codes are separated with spaces.
	 * 
	 * <p>
	 * Hidden, Incoherent or inexistent codes are removed from the list. Then the result list can be empty.
	 * 
	 * @param codes
	 * @return the references lines, or an empty list if none are found. Never return null.
	 */
	public List<ReferenceLine> getPublicAttributeLines(String codes) {
		if ((codes == null) || (codes.length() == 0)) {
			return new ArrayList<ReferenceLine>();
		}
		String[] codestab = codes.split(" "); //$NON-NLS-1$
		if ((codestab == null) || (codestab.length == 0)) { // blindage
			return new ArrayList<ReferenceLine>();
		}
		List<ReferenceLine> result = new ArrayList<ReferenceLine>(codestab.length);
		for (String code : codestab) {
			ReferenceLine rl = getAttributeLine(code);
			if ((rl != null) && !rl.isHidden()) {
				result.add(rl);
			}
		}
		return result;
	}

	/**
	 * The Type is a key that unically define this Entity.
	 * In other words, the type define the primary key of this entity structure.
	 * 
	 * @return the structure type.
	 */
	public String getType() {
		return type;
	}

	public boolean equalsType(ITypedBean bm) {
		if (type == null) {
			return bm.getType() == null;
		}
		return type.equals(bm.getType());
	}

	/**
	 * The Name is the printable, translated name of this entity.
	 * 
	 * <p>
	 * You may use this value in any conversation with the user.
	 * 
	 * <p>
	 * <b>Note that on server side the Entity are not translated. You need to clone it with desired language first.</b>
	 * 
	 * @return the translated name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * The Name is the printable, translated name of this entity.
	 * 
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * The Description of the Entity is a printable, user readable, text that can explain the nature of this Entity.
	 * 
	 * <p>
	 * You may use this value in any conversation with the user.
	 * 
	 * <p>
	 * <b>Note that on server side the Entity are not translated. You need to clone it with desired language first.</b>
	 * 
	 * 
	 * @return the translated description.
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * The Description of the Entity is a printable, user readable, text that can explain the nature of this Entity.
	 * 
	 * @param description
	 */
	public void setDescription(String description) {
		this.description = description;
	}
	
	/**
	 * @return The metadata part of this entity.
	 */
	public BeanMap getMetadata() {
		return metadata;
	}

	/**
	 * Return the mapper associated to this entity.
	 * 
	 * <p>
	 * This can return null if the entity is <i>currently</i> not associated to any mapper. This can append if the
	 * mapper service is not yet started or if this entity is a foreign entity (not stored into this server).
	 * 
	 * @return the BeanMapper service or null if none.
	 */
	public IMapperService getMapper() {
		if ((mapper == null) && (domain != null)) {
			// Initialize the Mapper...
			mapper = Activator.getInstance().getBeanMapper(domain);
		}
		return mapper;
	}

	/**
	 * @return the entities registry that own this entity.
	 */
	public IEntityRegistry getRegistry() {
		return registry;
	}

	/**
	 * For internal purpose only, Should not be used from other bundles.
	 * 
	 * @param metadata
	 */
	public void setMetadata(BeanMap metadata) {
		this.metadata = metadata;
	}

	/**
	 * For internal purpose only, Should not be used from other bundles.
	 * 
	 * @param mapper
	 */
	public void setMapper(IMapperService mapper) {
		this.mapper = mapper;
	}

	/**
	 * For internal purpose only, Should not be used from other bundles.
	 * 
	 * @param registry
	 */
	public void setRegistry(IEntityRegistry registry) {
		this.registry = registry;
	}

	/**
	 * @param domain
	 *            the domain relative to the mapper of this entity.
	 */
	public void setDomain(String domain) {
		this.domain = domain;
	}

	/**
	 * @return the domain relative to the mapper of this entity.
	 */
	public String getDomain() {
		return domain;
	}

	/**
	 * Return true if the two entities share the same mapper.
	 * 
	 * @param entity
	 * @return
	 */
	public boolean sameMapper(MetaDataEntity entity) {
		return getMapper() == entity.getMapper();
	}

	/**
	 * @param version
	 *            the version number of this entity declaration.
	 */
	public void setVersion(int version) {
		this.version = version;
	}

	/**
	 * @return the version number of this entity declaration.
	 */
	public int getVersion() {
		return version;
	}

	/**
	 * This criteria must be tested before item creation. It can not be applied to existing data.
	 * 
	 * <p>
	 * If this entity does not define a specific right for the creation operation, then this is
	 * the update right that it is used. If no update right is defined then this is the read 
	 * right that it is used.
	 * 
	 * <p>
	 * Note that read-only entities always return false.
	 * 
	 * @return the creation right criteria applied to this entity.
	 * @see #getRightRead()
	 */
	public ISearchCriteria getRightCreate() {
		if (isReadOnly()) {
			return ConstantCriteria.FALSE;
		}
		if (create != null) {
			return create;
		}
		if (update != null) {
			return update;
		}
		return getRightRead();
	}

	/**
	 * This right must be tested before item modification, it must be applied to an existing data.
	 * 
	 * <p>
	 * If this entity does not define a specific right for the update operation, then this is
	 * the creation right that it is used. If no creation right is defined then this is the read 
	 * right that it is used.
	 * 
	 * <p>
	 * Note that read-only entities always return false.
	 * 
	 * @return the update right criteria applied to this entity.
	 * @see #getRightRead()
	 */
	public ISearchCriteria getRightUpdate() {
		if (isReadOnly()) {
			return ConstantCriteria.FALSE;
		}
		if (update != null) {
			return update;
		}
		if (create != null) {
			return create;
		}
		return getRightRead();
	}

	/**
	 * This right must be tested before item access, it must be applied to an existing data.
	 * 
	 * <p>
	 * This right must not be used if the access is a selection (with complex conditional part and multiples result).
	 * Basically reading access belong to a data process, selection belong to a items search or general listing.
	 * 
	 * @return the read right criteria applied to this entity.
	 */
	public ISearchCriteria getRightRead() {
		if (read != null) {
			return read;
		}
		return ConstantCriteria.TRUE;
	}

	/**
	 * This right must be tested before item listing, it must be applied to existing datas but can not be applied to a
	 * unique id.
	 * 
	 * <p>
	 * This right must not be used if the access is a selection (with complex conditional part and multiples result).
	 * Basically selection access belong to a items search or general listing, reading ones belong to a data process.
	 * 
	 * @return the list right criteria applied to this entity.
	 */
	public ISearchCriteria getRightList() {
		if (list != null) {
			return list;
		}
		return getRightRead();
	}

	/**
	 * This right must be tested before item deletion, it must be applied to an existing data.
	 * 
	 * <p>
	 * if this entity does not define a Delete right then this is the update right that is is returned.
	 * If none update right is defined then this is the creation right, and finally the read right.
	 * 
	 * @return the delete right criteria applied to this entity.
	 * @see #getRightUpdate()
	 */
	public ISearchCriteria getRightDelete() {
		if (isReadOnly()) {
			return ConstantCriteria.FALSE;
		}
		if (delete != null) {
			return delete;
		}
		return getRightUpdate();
	}

	/**
	 * @return the simple creation criteria without deduction.
	 * @see #getRightCreate()
	 */
	public ISearchCriteria getCreate() {
		return create;
	}

	/**
	 * @return the simple update criteria without deduction.
	 * @see #getRightUpdate()
	 */
	public ISearchCriteria getUpdate() {
		return update;
	}

	/**
	 * @return the simple read criteria without deduction.
	 * @see #getRightRead()
	 */
	public ISearchCriteria getRead() {
		return read;
	}

	/**
	 * @return the simple list criteria without deduction.
	 * @see #getRightList()
	 */
	public ISearchCriteria getList() {
		return list;
	}

	/**
	 * @return the simple delete criteria without deduction.
	 * @see #getRightDelete()
	 */
	public ISearchCriteria getDelete() {
		return delete;
	}

	/**
	 * <b>For internal purpose only, Should not be used from other bundles.</b>
	 * 
	 * @param create
	 */
	public void setCreate(ISearchCriteria create) {
		this.create = create;
	}

	/**
	 * <b>For internal purpose only, Should not be used from other bundles.</b>
	 * 
	 * @param update
	 */
	public void setUpdate(ISearchCriteria update) {
		this.update = update;
	}

	/**
	 * <b>For internal purpose only, Should not be used from other bundles.</b>
	 * 
	 * @param read
	 */
	public void setRead(ISearchCriteria read) {
		this.read = read;
	}

	/**
	 * <b>For internal purpose only, Should not be used from other bundles.</b>
	 * 
	 * @param list
	 */
	public void setList(ISearchCriteria list) {
		this.list = list;
	}

	/**
	 * <b>For internal purpose only, Should not be used from other bundles.</b>
	 * 
	 * @param delete
	 */
	public void setDelete(ISearchCriteria delete) {
		this.delete = delete;
	}

	/**
	 * @return true if this entity define at least one right access rule.
	 */
	public boolean hasRights() {
		return (create != null) || (update != null) || (read != null) || (list != null) || (delete != null);
	}

	private boolean canDo(IConnectionUserBean user, ISearchCriteria criteria, int id, boolean internal) {
		if ((criteria == null) || ConstantCriteria.TRUE.equals(criteria)) {
			return true;
		}
		if ((user == null) || ConstantCriteria.FALSE.equals(criteria)) {
			return false;
		}
		criteria = criteria.reduce(new CriteriaContextBasic(this, user));
		if ((criteria == null) || ConstantCriteria.TRUE.equals(criteria)) {
			return true;
		}
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return false;
		}
		if (internal) {
			return true;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			return false;
		}
		if (id == 0) {
			return dataTest(criteria, user);
		}
		return dataTest(id, criteria, user);
	}
	
	/**
	 * Test if the current user can create a data of this kind of entity.
	 * 
	 * <p>
	 * if the parameter <code>internal</code> is true then the program try to answer
	 * to this question without accessing to the server (for client program) nor to the
	 * database (for server program). In this case if the access test need some information
	 * from the data storage, the methode result is <b>true</b>, then this answer may be
	 * overrided when the user will effectivelly try to execute it.
	 *  
	 * @param user The current user that want to release the operation.
	 * @param internal If true the no external call (to database nor server) will be made.
	 * @return false if the current user can not perform this kind of operation.
	 */
	public boolean canCreate(IConnectionUserBean user, boolean internal) {
		return canDo(user, getRightCreate(), 0, internal);
	}

	/**
	 * Test if the current user can list datas of this kind of entity. 
	 * 
	 * <p>
	 * if the parameter <code>internal</code> is true then the program try to answer
	 * to this question without accessing to the server (for client program) nor to the
	 * database (for server program). In this case if the access test need some information
	 * from the data storage, the method result is <b>true</b>, then this answer may be
	 * contradicted when the user will effectively try to execute it.
	 *  
	 * @param user The current user that want to release the operation.
	 * @param internal If true the no external call (to database nor server) will be made.
	 * @return false if the current user can not perform this kind of operation.
	 */
	public boolean canList(IConnectionUserBean user, boolean internal) {
		return canDo(user, getRightList(), 0, internal);
	}

	/**
	 * Test if the current user can update the targeted data. 
	 * 
	 * <p>
	 * if the parameter <code>internal</code> is true then the program try to answer
	 * to this question without accessing to the server (for client program) nor to the
	 * database (for server program). In this case if the access test need some information
	 * from the data storage, the method result is <b>true</b>, then this answer may be
	 * contradicted when the user will effectively try to execute it.
	 *  
	 * @param user The current user that want to release the operation.
	 * @param id The internal identifier of the target data (only required if <code>internal</code> is false).
	 * @param internal If true the no external call (to database nor server) will be made.
	 * @return false if the current user can not perform this kind of operation.
	 */
	public boolean canUpdate(IConnectionUserBean user, int id, boolean internal) {
		return canDo(user, getRightUpdate(), id, internal);
	}

	/**
	 * Test if the current user can read the targeted data.
	 * 
	 * <p>
	 * if the parameter <code>internal</code> is true then the program try to answer
	 * to this question without accessing to the server (for client program) nor to the
	 * database (for server program). In this case if the access test need some information
	 * from the data storage, the method result is <b>true</b>, then this answer may be
	 * contradicted when the user will effectively try to execute it.
	 *  
	 * @param user The current user that want to release the operation.
	 * @param id The internal identifier of the target data (only required if <code>internal</code> is false).
	 * @param internal If true the no external call (to database nor server) will be made.
	 * @return false if the current user can not perform this kind of operation.
	 */
	public boolean canRead(IConnectionUserBean user, int id, boolean internal) {
		return canDo(user, getRightRead(), id, internal);
	}

	/**
	 * Test if the current user can delete the targeted data.
	 * 
	 * <p>
	 * if the parameter <code>internal</code> is true then the program try to answer
	 * to this question without accessing to the server (for client program) nor to the
	 * database (for server program). In this case if the access test need some information
	 * from the data storage, the method result is <b>true</b>, then this answer may be
	 * contradicted when the user will effectively try to execute it.
	 *  
	 * @param user The current user that want to release the operation.
	 * @param id The internal identifier of the target data (only required if <code>internal</code> is false).
	 * @param internal If true the no external call (to database nor server) will be made.
	 * @return false if the current user can not perform this kind of operation.
	 */
	public boolean canDelete(IConnectionUserBean user, int id, boolean internal) {
		return canDo(user, getRightDelete(), id, internal);
	}
	
	/**
	 * Inject all the properties of the given entity into the current object.
	 * 
	 * <p>
	 * This does not change the entity type. The version number can only be augmented (if the parameter <code>purge</code> is true.
	 * 
	 * <p>
	 * If the parameter <code>purge</code> is true then the existing Metadatas, attributes, links and tests are cleared before injection.
	 * 
	 * <p>
	 * Null attributes are not injected.
	 * 
	 * @param entity
	 *            an update of the current object.
	 * @param
	 * @return this.
	 */
	public MetaDataEntity inject(MetaDataEntity entity, boolean purge) {
		// Le Type n'est jamais injecté (car il défini l'entité !)
		if (purge && (entity.version > 0) && ((version == 0) || (version < entity.version))) {
			version = entity.version;
		}
		if ((entity.date != null) && ((date == null) || date.before(entity.date))) {
			date = entity.date;
		}
		if (entity.lockable != null) {
			lockable = entity.lockable;
		}
		if ((entity.groupType != null) && (entity.groupType.length() > 0)) {
			groupType = entity.groupType;
		}
		if (entity.readonly != null) {
			readonly = entity.readonly;
		}
		if ((entity.domain != null) && (entity.domain.length() > 0)) {
			domain = entity.domain;
		}
		if (entity.mapper != null) {
			mapper = entity.mapper;
		}
		if (entity.registry != null) {
			registry = entity.registry;
		}
		// injection des droits
		if (entity.create != null) {
			create = entity.create;
		}
		if (entity.delete != null) {
			delete = entity.delete;
		}
		if (entity.list != null) {
			list = entity.list;
		}
		if (entity.read != null) {
			read = entity.read;
		}
		if (entity.update != null) {
			update = entity.update;
		}
		if (purge) {
			metadata.clear();
			attributes.clear();
			links.clear();
			tests.clear();
		}
		metadata.addAll(entity.metadata);
		for (MetaDataTest test : entity.getTests().values()) {
			MetaDataTest t = tests.get(test.getCode());
			if (t == null) {
				tests.put(test.getCode(), new MetaDataTest(test, this));
			} else {
				// TODO Does not fulfill the null Boolean values.
				t.injectValues(test);
			}
		}
		for (MetaDataLink link : entity.getLinks().values()) {
			MetaDataLink l = getLink(link.getCode());
			if (l == null) {
				getLinks().put(link.getCode(), new MetaDataLink(link, this));
			} else {
				if (link.getRightCreate(false) != null) {
					l.setCreate(link.getRightCreate(false));
				}
				if (link.getRightList(false) != null) {
					l.setList(link.getRightList(false));
				}
				l.getMetadata().addAll(link.getMetadata());
				if (link.getName() != null) {
					l.setName(link.getName());
				}
				if (link.getDescription() != null) {
					l.setDescription(link.getDescription());
				}
				if ((link.getType() != null) && (link.getType().length() > 0)) {
					l.setType(link.getType());
				}
				if (link.getTest() != null) {
					l.setTest(link.getTest());
				}
				if (link.getReadOnly() != null) {
					l.setReadonly(link.getReadOnly());
				}
			}
		}
		for (MetaDataAttribute att : entity.getAttributes().values()) {
			MetaDataAttribute a = getAttribute(att.getCode());
			if (a == null) {
				getAttributes().put(att.getCode(), new MetaDataAttribute(att, this));
			} else {
				if ((att).getRightRead(false) != null) {
					a.setRead((att).getRightRead(false));
				}
				if ((att).getRightUpdate(false) != null) {
					a.setUpdate((att).getRightUpdate(false));
				}
				a.getMetadata().addAll((att).getMetadata());
				if (att.getName() != null) {
					a.setName(att.getName());
				}
				if (att.getDescription() != null) {
					a.setDescription(att.getDescription());
				}
				if ((att.getType() != null) && (att.getType().length() > 0)) {
					a.setType(att.getType());
				}
				if (att.getTest() != null) {
					a.setTest(att.getTest());
				}
				if (att.getReadOnly() != null) {
					a.setReadonly(att.getReadOnly());
				}
				if (att.getLength() != 0) {
					a.setLength(att.getLength());
				}
				if (att.getPrecision() != 0) {
					a.setPrecision(att.getPrecision());
				}
				if (att.getListable() != null) {
					a.setListable(att.getListable());
				}
				if (att.getMandatory() != null) {
					a.setMandatory(att.getMandatory());
				}
			}
		}
		return this;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof MetaDataEntity) {
			return (version == ((MetaDataEntity) obj).version) && //
					(type != null) && //
					type.equals(((MetaDataEntity)obj).type);
		}
		return super.equals(obj);
	}
	
	public int compareTo(MetaDataEntity o) {
		if (type != null) {
			int i = type.compareTo(o.type);
			if (i != 0) {
				return i;
			}
		}
		return version - o.version;
	}

	/**
	 * @return the last structural modification change.
	 */
	@Override
	public Date getDate() {
		return date;
	}

	@Override
	public void setDate(Date date) {
		this.date = date;
	}

	@Override
	public int getMUID() {
		return 0;
	}

	@Override
	public void setMUID(int id) {}

	@Override
	public void setModification(int uid, Date date) {
		setDate(date);
	}

	@Override
	public boolean moreRecent(IDatedBean bm) {
		if (date == null) {
			return bm.getDate() == null;
		}
		if (bm.getDate() == null) {
			return true;
		}
		return date.after(bm.getDate());
	}

	/**
	 * Set the whole attribute Hash table.
	 * 
	 * <p>For internal use only.
	 * 
	 * @param attributes
	 */
	public void setAttributes(HashMap<String, MetaDataAttribute> attributes) {
		this.attributes = attributes;
	}

	/**
	 * Return the attributes of this entity that are listables and public (i.e. not hidden). The
	 * returned list is a list of ReferenceLine to be proceeded by mappers.
	 * 
	 * @return a non null list of reference lines..
	 */
	public List<ReferenceLine> getListables() {
		List<ReferenceLine> result = new ArrayList<ReferenceLine>(getAttributes().size());
		for (MetaDataAttribute att : getAttributes().values()) {
			if (att.isListable() && att.isPublic()) {
				result.add(new ReferenceLine(att));
			}
		}
		return result;
	}

	/**
	 * Return the list of "listable" and public attributes.
	 * 
	 * <p>Theses attributes are the default ones returned when asking for a list of items.
	 * 
	 * @return a non null list of attributes.
	 */
	public List<MetaDataAttribute> getListableAttributes() {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(attributes.size());
		for(MetaDataAttribute att:attributes.values()) {
			if (att.isListable() && att.isPublic()) {
				result.add(att);
			}
		}
		return result;
	}

	/**
	 * Construct a list of selectable attributes from all the attributes declared into this
	 * Entity plus the required ones.
	 * 
	 * @param codes none, one or more code of attributes related to this entity. 
	 * @return All the attributes of this entity plus the required ones.
	 */
	public List<ReferenceLine> getAllAttributes(String... codes) {
		List<ReferenceLine> result = new ArrayList<ReferenceLine>(getAttributes().size());
		for (MetaDataAttribute att : attributes.values()) {
			result.add(new ReferenceLine(att));
		}
		for (String code: codes) {
			result.add(getReferenceLine(code));
		}
		return result;
	}

	/**
	 * Construct a list of public attributes from all the attributes declared into this
	 * Entity plus the required ones.
	 * 
	 * @param codes none, one or more code of attributes related to this entity. 
	 * @return All the attributes of this entity plus the required ones.
	 */
	public List<ReferenceLine> getAllPublicAttributes(String... codes) {
		List<ReferenceLine> result = new ArrayList<ReferenceLine>(getAttributes().size());
		for (MetaDataAttribute att : attributes.values()) {
			if (att.isPublic()) {
				result.add(new ReferenceLine(att));
			}
		}
		for(String code: codes) {
			ReferenceLine rl = getReferenceLine(code);
			if (!rl.isHidden()) {
				result.add(rl);
			}
		}
		return result;
	}

	/**
	 * Get the attributes codes separated by spaces.
	 * 
	 * @return The list of all the attributes codes.
	 */
	public String getAttributeCodes() {
		StringBuilder sb = new StringBuilder();
		boolean spaces = false;
		for (MetaDataAttribute att : attributes.values()) {
			if (spaces) {
				sb.append(' ');
			} else {
				spaces = true;
			}
			sb.append(att.getCode());
		}
		return sb.toString();
	}

	/**
	 * Build the list of attributes corresponding to the given code list.
	 * 
	 * <p>
	 * Does not support references (codes with dots).
	 * 
	 * @param codes
	 *            an attributes codes list, separated with spaces.
	 * @return the corresponding list of attributes.
	 * @see #getAttributeLines(String)
	 */
	public List<MetaDataAttribute> getAttributes(String codes) {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>();
		if (codes == null) {
			return result;
		}
		for (String code : codes.split(" ")) { //$NON-NLS-1$
			MetaDataAttribute att = getAttribute(code);
			if (att != null) {
				result.add(att);
			}
		}
		return result;
	}

	/**
	 * Build the list of public attributes corresponding to the given code list.
	 * 
	 * <p>
	 * Does not support references (codes with dots).
	 * 
	 * @param codes
	 *            an attributes codes list, separated with spaces.
	 * @return the corresponding list of attributes.
	 * @see #getAttributeLines(String)
	 */
	public List<MetaDataAttribute> getPublicAttributes(String codes) {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>();
		if (codes == null) {
			return result;
		}
		for (String code : codes.split(" ")) { //$NON-NLS-1$
			MetaDataAttribute att = getAttribute(code);
			if ((att != null) && att.isPublic()) {
				result.add(att);
			}
		}
		return result;
	}

	/**
	 * Get the Attribute hash table.
	 * @return
	 */
	public HashMap<String, MetaDataAttribute> getAttributes() {
		return attributes;
	}

	/**
	 * Build the list of attributes corresponding to the given code list.
	 * 
	 * <p>
	 * Does not support references (codes with dots).
	 * 
	 * @param codes
	 *            an attributes codes list.
	 * @return the corresponding list of attributes.
	 * @see #getAttributeLines(String)
	 */
	public List<MetaDataAttribute> getAttributes(String[] codes) {
		if (codes == null) {
			return new ArrayList<MetaDataAttribute>();
		}
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(codes.length);
		for (String code : codes) {
			MetaDataAttribute att = getAttribute(code);
			if (att != null) {
				result.add(att);
			}
		}
		return result;
	}

	/**
	 * Build the list of attributes corresponding to the given code list.
	 * 
	 * <p>
	 * Does not support references (codes with dots).
	 * 
	 * @param codes
	 *            an attributes codes list.
	 * @return the corresponding list of attributes.
	 * @see #getAttributeLines(String)
	 */
	public List<MetaDataAttribute> getPublicAttributes(String[] codes) {
		if (codes == null) {
			return new ArrayList<MetaDataAttribute>();
		}
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(codes.length);
		for (String code : codes) {
			MetaDataAttribute att = getAttribute(code);
			if ((att != null) && att.isPublic()) {
				result.add(att);
			}
		}
		return result;
	}

	/**
	 * Get the list of attributes used into this BeanMap.
	 * @param bean a BeanMap.
	 * @return a non null list of attributes.
	 */
	public List<MetaDataAttribute> getAttributes(BeanMap bean) {
		if (bean == null) {
			return new ArrayList<MetaDataAttribute>();
		}
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(bean.size());
		for (String code : bean.keys()) {
			MetaDataAttribute att = getAttribute(code);
			if (att != null) {
				result.add(att);
			}
		}
		return result;
	}
	
	/**
	 * Return the list of attributes that possess the given type.
	 * @param type An attribute type.
	 * @return
	 */
	public List<MetaDataAttribute> getAttributesFromType(String type) {
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>();
		if (type != null) {
			for(MetaDataAttribute att : attributes.values()) {
				if (type.equals(att.getType())) {
					result.add(att);
				}
			}
		}
		return result;
	}

	/**
	 * Return the ordered list of attributes of this structure.
	 * 
	 * <p>The attributes are ordered according to their <code>ColRank</code> property.
	 * 
	 * @param onlyListables Only Listable attributes will be returned.
	 * @param onlyVisibles Only Visible attributes will be returned.
	 * @return a non null collection of attributes.
	 */
	public Collection<MetaDataAttribute> getOrderedAttributes(boolean onlyListables, boolean onlyVisibles) {
		return getOrderedAttributes(onlyListables, onlyVisibles, false);
	}
	
	/**
	 * Return the ordered list of attributes of this structure.
	 * 
	 * <p>The attributes are ordered according to their <code>ColRank</code> property.
	 * 
	 * @param onlyListables Only Listable attributes will be returned.
	 * @param onlyVisibles Only Visible attributes will be returned.
	 * @param onlyPublic Only not hidden attributes will be returned.
	 * @return
	 */
	public Collection<MetaDataAttribute> getOrderedAttributes(boolean onlyListables, boolean onlyVisibles, boolean onlyPublic) {
		Iterator<MetaDataAttribute> itt = attributes.values().iterator();
		if (!itt.hasNext()) {
			return new ArrayList<MetaDataAttribute>();
		}
		ArrayList<MetaDataAttribute> result = new ArrayList<MetaDataAttribute>(attributes.size());
		result.add(itt.next());
		nexta:while(itt.hasNext()) {
			MetaDataAttribute a = itt.next();
			if ((a.isListable() || !onlyListables) &&
				(!onlyVisibles || a.isVisible()) &&
				(!onlyPublic || a.isPublic())) {
				int cr = a.getMetadata().getInt(METADATA_COLRANK);
				for(int i = result.size() -1; i>=0 ; i--) {
					int icr = result.get(i).getMetadata().getInt(METADATA_COLRANK);
					if (icr < cr) {
						if (i == result.size() -1) {
							result.add(a);
						} else {
							result.add(i+1, a);
						}
						continue nexta;
					}
				}
				result.add(0, a);
			}
		}
		return result;
	}

	/**
	 * Convert the given BeanMap into another one which is compliant with the constrains of this entity. Allowing to
	 * use it to create or update the data. 
	 * 
	 * <p>
	 * First, build the attributes list corresponding to the codes used into the original BeanMap.
	 * Second, store the converted attributes values according to the attributes types.
	 * 
	 * <p>
	 * References line, read-only and translatable attributes are ignored because theses attributes are not supposed to
	 * be updated.
	 * 
	 * @param bean
	 *            an BeanMap.
	 * @param attributes
	 *            the list to store the attributes of this entity used into the given form. May be null if this list is
	 *            not used.
	 * @return a BeanMap containing the attributes converted values and the other properties that do not correspond to
	 *         attributes.
	 */
	public BeanMap filterBean(BeanMap bean, List<MetaDataAttribute> attributes) {
		return filterBean(bean, attributes, false, false, false);
	}
	
	/**
	 * Convert the given BeanMap into another one which is compliant with the constrains of this entity. Allowing to
	 * use it to create or update the data. 
	 * 
	 * <p>
	 * First, build the attributes list corresponding to the codes used into the original BeanMap.
	 * Second, store the converted attributes values according to the attributes types.
	 * 
	 * <p>
	 * References line, read-only and translatable attributes are ignored because theses attributes are not supposed to
	 * be updated.
	 * 
	 * @param bean
	 *            an BeanMap.
	 * @param attributes
	 *            the list to store the attributes of this entity used into the given form. May be null if this list is
	 *            not used.
	 * @param references
	 *            If true the references lines are merged into a sub BeanMap stored in the result.
	 * @param readonly
	 *            If true the read-only attributes are included in the result.
	 * @param tranlatable
	 *            If true the translatable attributes are included in the result.
	 * @return a BeanMap containing the attributes converted values without the other properties that do not correspond to
	 *         attributes.
	 */
	public BeanMap filterBean(BeanMap bean, List<MetaDataAttribute> attributes, boolean references, boolean readOnly, boolean translatable) {
		BeanMap result = new BeanMap(getType());
		if ((bean.getType() != null) && !type.equals(bean.getType())) {
			return result;
		}
		for (Entry<String, Object> e: bean.entrySet()) {
			MetaDataAttribute att = getAttribute(e.getKey());
			if (att == null) {
				if (references) {
					int i = e.getKey().indexOf('.');
					if (i > 0) {
						String acode = e.getKey().substring(0, i);
						att = getAttribute(acode);
						if (att != null) {
							ReferenceLine rl = getAttributeLine(e.getKey());
							if ((rl != null) && (rl.getLastAttribute() != null)) {
								BeanMap sbm = result.getBeanMap(acode);
								if (sbm == null) {
									if (attributes != null) {
										attributes.add(att);
									}
									sbm = new BeanMap(att.getType());
									result.put(acode, sbm);
								}
								if (e.getValue() == null) {
									sbm.put(e.getKey().substring(i + 1), null);
								} else {
									sbm.put(e.getKey().substring(i + 1), rl.getLastAttribute().convertValue(e.getValue().toString()));
								}
								continue;
							}
						}
					}
				}
			} else if (((!att.isReadonly()) || readOnly) && //
					(e.getKey().indexOf('.') == -1) && //
					((!att.isTranslatable()) || translatable)) {
				// We ignore read-only attributes (plus any composed attribute that is
				// not explicitly declared as read-only).
				if (attributes != null) {
					attributes.add(att);
				}
				if (e.getValue() == null) {
					result.put(e.getKey(), null);
				} else {
					result.put(e.getKey(), att.convertValue(e.getValue().toString()));
				}
			}
		}
		return result;
	}

	/**
	 * Execute the conversion in one step. First, build the attributes list corresponding to the codes used into the Form.
	 * Second, create a BeanMap to store the converted attributes values according to the attributes types.
	 * 
	 * <p>
	 * References line, read-only and translatable attributes are ignored because theses attributes are not supposed to
	 * be updated.
	 * 
	 * @param form
	 *            an Request form.
	 * @param attributes
	 *            the list to store the attributes of this entity used into the given form. May be null if this list is
	 *            not used.
	 * @return a BeanMap containing the attributes converted values and the other properties that do not correspond to
	 *         attributes.
	 */
	public BeanMap formToBean(Form form, List<MetaDataAttribute> attributes) {
		return formToBean(form, attributes, false, false, false);
	}
	
	/**
	 * Execute the conversion in one step. First, build the attributes list corresponding to the codes used into the Form.
	 * Second, create a BeanMap to store the converted attributes values according to the attributes types.
	 * 
	 * @param form
	 *            an Request form.
	 * @param attributes
	 *            the list to store the attributes of this entity used into the given form. May be null if this list is
	 *            not used.
	 * @param references
	 *            If true the references lines are merged into a sub BeanMap stored in the result.
	 * @param readonly
	 *            If true the read-only attributes are included in the result.
	 * @param tranlatable
	 *            If true the translatable attributes are included in the result.
	 * @return a BeanMap containing the attributes converted values and the other properties that do not correspond to
	 *         attributes.
	 */
	public BeanMap formToBean(Form form, List<MetaDataAttribute> attributes, boolean references, boolean readOnly, boolean translatable) {
		BeanMap result = new BeanMap(getType());
		for (String code : form.getNames()) {
			MetaDataAttribute att = getAttribute(code);
			String value = form.getFirstValue(code);
			if (att == null) {
				if (references) {
					int i = code.indexOf('.');
					if (i > 0) {
						String acode = code.substring(0, i);
						att = getAttribute(acode);
						if (att != null) {
							ReferenceLine rl = getAttributeLine(code);
							if ((rl != null) && (rl.getLastAttribute() != null)) {
								BeanMap sbm = result.getBeanMap(acode);
								if (sbm == null) {
									sbm = new BeanMap(att.getType());
									result.put(acode, sbm);
								}
								sbm.put(code.substring(i + 1), rl.getLastAttribute().convertValue(value));
								continue;
							}
						}
					}
				}
				// We insert parameter that are not attributes into the beanmap.
				// This allow further procedure to get some additional parameter that will
				// not be taken into account of insertion (based on the returned MetaData).
				if (att == null) {
					result.put(code, value);
				}
			} else if (((!att.isReadonly()) || readOnly) && //
					(code.indexOf('.') == -1) && //
					((!att.isTranslatable()) || translatable)) {
				// We ignore read-only attributes (plus any composed attribute that is
				// not explicitly declared as read-only).
				if (attributes != null) {
					attributes.add(att);
				}
				result.put(code, att.convertValue(value));
			}
		}
		return result;
	}

	/**
	 * Build a list of values according to the given Attributes list and BeanMap item.
	 * 
	 * <p>
	 * The attributes a not necessarily part of the current entity.
	 * 
	 * @param list
	 *            a list of attributes.
	 * @param item
	 *            a BeanMap Item containing some values.
	 * @return the list of values corresponding to the given attributes. In the same order and same length, some values
	 *         can be null.
	 */
	public List<Object> getValues(List<MetaDataAttribute> list, BeanMap item) {
		ArrayList<Object> result = new ArrayList<Object>(list.size());
		for (MetaDataAttribute att : list) {
			result.add(item.get(att.getCode()));
		}
		return result;
	}

	/**
	 * Build a list of values according to the given attributes codes and BeanMap item.
	 * 
	 * @param codes
	 *            a list of attributes codes separated with spaces.
	 * @param item
	 *            a BeanMap Item containing some values.
	 * @return the list of values corresponding to the given attribute codes. In the same order, some values can be
	 *         null.
	 */
	public List<Object> getValues(String codes, BeanMap item) {
		return getValues(getAttributes(codes), item);
	}

	/**
	 * First get the values of the given attributes codes from the BeanMap item. And then fill the attributes list with
	 * the corresponding attributes.
	 * 
	 * @param codes
	 *            a list of attributes codes separated with spaces.
	 * @param item
	 *            a BeanMap Item containing some values.
	 * @param attributes
	 *            a <b> not null</b> list of attributes that will be filled with found codes.
	 * @return the list of values corresponding to the given attribute codes. In the same order, some values can be
	 *         null.
	 */
	public List<Object> getValues(String codes, BeanMap item, List<MetaDataAttribute> attributes) {
		if (codes == null) {
			return new ArrayList<Object>();
		}
		String[] codestab = codes.split(" "); //$NON-NLS-1$
		ArrayList<Object> result = new ArrayList<Object>(codestab.length);
		for (String code : codestab) {
			MetaDataAttribute att = getAttribute(code);
			if (att != null) {
				attributes.add(att);
				result.add(item.get(code));
			}
		}
		return result;
	}

	/**
	 * First get the values from the BeanMap item. And then fill the attributes list with the corresponding attributes.
	 * 
	 * <p>
	 * The values form the BeanMap that do not correspond to any attributes are ignored.
	 * 
	 * @param item
	 *            a BeanMap Item containing some values.
	 * @param attributes
	 *            a <b> not null</b> list of attributes that will be filled with found codes.
	 * @return the list of values corresponding to the given attribute codes, in the same order.
	 */
	public List<Object> getValues(BeanMap item, List<MetaDataAttribute> attributes) {
		ArrayList<Object> result = new ArrayList<Object>(item.size());
		for (String code : item.keys()) {
			MetaDataAttribute att = getAttribute(code);
			if ((att != null) && !attributes.contains(att)) {
				attributes.add(att);
				result.add(item.get(code));
			}
		}
		return result;
	}

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param attributes
	 *            the list of updated attributes codes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap dataCreate(String attributes, Object... values) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCreation,getType()));
		}
		return mapper.create(this, getAttributes(attributes), AbstractMapperService.list(values));
	}

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param beanMap
	 *            The beanMap that you want to store
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap dataCreate(BeanMap beanMap) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCreation,getType()));
		}
		List<MetaDataAttribute> attlist = new ArrayList<MetaDataAttribute>();
		return mapper.create(this,attlist,getValues(beanMap, attlist));
	}	

	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * @param beanMap
	 *            The beanMap that you want to store
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap dataCreate(BeanMap beanMap, IConnectionUserBean currentUser) {
		if (beanMap == null) {
			return null;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCreation,getType()));
		}
		if ((beanMap.getType() != null) && !type.equals(beanMap.getType())) {
			Activator.getInstance().warn("The Beanmap \"{}\" appears to be created with the wrong entity \"{}\".", beanMap.getType(), type);
		}
		List<MetaDataAttribute> attlist = new ArrayList<MetaDataAttribute>();
		return mapper.create(this, attlist, getValues(beanMap, attlist), currentUser);
	}	
	
	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * <p>
	 * This is the caller responsibility to ensure that the attributes list correspond to the values list.
	 * 
	 * @param attributes
	 *            the list of updated attributes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap dataCreate(List<MetaDataAttribute> attributes, List<Object> values) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCreation,getType()));
		}
		return mapper.create(this, attributes, values);
	}
	
	/**
	 * Create a new element into the storage base (This request return the new item created into the database).
	 * 
	 * <p>
	 * The returned BeanMap possess a positive id, used as internal reference for subsequent calls.
	 * 
	 * <p>
	 * This is the caller responsibility to ensure that the attributes list correspond to the values list.
	 * 
	 * @param attributes
	 *            the list of updated attributes
	 * @param values
	 *            the list of values in the same order than the attributes listed. some values can be null or need to be
	 *            translated before to get stored.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the new item in a BeanMap with attributes and id or null if an error occurs.
	 */
	public BeanMap dataCreate(List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCreation,getType()));
		}
		return mapper.create(this, attributes, values, currentUser);
	}

	/**
	 * Delete an item from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE,PUT nor GET REST services.
	 * 
	 * @param itemId
	 *            the item id to delete
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return true if the item exist and has been deleted.
	 * @see #dataUndelete(int)
	 */
	public boolean dataDelete(int itemId, boolean hardDelete) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.delete(this, itemId, hardDelete);
	}

	/**
	 * Delete an item from the storage.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion will be a soft deletion. Soft deletion allow the item to
	 * be undeleted.
	 * 
	 * @param itemId
	 *            the item id to delete
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the item exist and has been deleted.
	 * @see #dataUndelete(int)
	 */
	public boolean dataDelete(int itemId, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.delete(this, itemId, false, currentUser);
	}

	/**
	 * Delete a set of data according to the given condition.
	 * 
	 * <p>
	 * If the mapper allow this operation, the deletion can be a soft or hard deletion. Soft deletion allow the item to
	 * be undeleted. Hard deletion are not idempotent, multiples calls to theses method should not possible from
	 * DELETE,PUT nor GET REST services.
	 * 
	 * @param criteria
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param hardDelete
	 *            True if the deletion must be an "hard deletion".
	 * @return the number of deleted elements.
	 */
	public int dataDelete(ISearchCriteria criteria, IConnectionUserBean currentUser, boolean hardDelete) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.delete(this, criteria, currentUser, hardDelete);
	}
	
	/**
	 * Cancel a soft deletion operation.
	 * 
	 * <p>
	 * Hard deletion are not reversible.
	 * 
	 * @param itemId
	 *            the item id to undelete
	 * @return True if the item has been successfully undeleted.
	 * @see #delete(MetaDataEntity, int, boolean)
	 */
	public boolean dataUndelete(int itemId) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.undelete(this, itemId);
	}
	
	/**
	 * Cancel a soft deletion operation.
	 * 
	 * <p>
	 * Hard deletion are not reversible.
	 * 
	 * @param itemId
	 *            the item id to undelete
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return True if the item has been successfully undeleted.
	 * @see #delete(MetaDataEntity, int, boolean)
	 */
	public boolean dataUndelete(int itemId, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.undelete(this, itemId, currentUser);
	}

	/**
	 * Cancel a soft deletion of a set of elements.
	 * 
	 * @param criteria
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of undeleted elements.
	 */
	public int dataUndelete(ISearchCriteria criteria, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataDeletion,getType()));
		}
		return mapper.undelete(this, criteria, currentUser);
	}

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean dataUpdate(int itemId, String attributes, Object... values) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		if ((attributes == null) || (attributes.length() == 0)) {
			return mapper.update(this, itemId, null);
		}
		return mapper.update(this, itemId, attributes, AbstractMapperService.list(values));
	}

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param beanMap
	 *            The beanMap to update. Note that all the defined attributes will be updated into the database
	 * @return true if the item has been successfully updated.
	 */
	public boolean dataUpdate(BeanMap beanMap) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		if (beanMap.isEmpty()) {
			return mapper.update(this, beanMap.getId(), null);
		}
		return mapper.update(beanMap);
	}

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param beanMap
	 *            The beanMap to update. Note that all the defined attributes will be updated into the database
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the item has been successfully updated.
	 */
	public boolean dataUpdate(BeanMap beanMap, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		if (beanMap.isEmpty()) {
			return mapper.update(this, beanMap.getId(), null, currentUser);
		}
		return mapper.update(beanMap, currentUser);
	}
	
	/**
	 * Update a set of items into the storage.
	 * 
	 * <p>
	 * Update operation can be applied to (soft) deleted items.
	 * 
	 * @param attributes
	 *            The list of attributes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param criteria
	 *            A conditional selection of item to update.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if some of the items have been successfully updated.
	 */
	public boolean dataUpdate(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate, getType()));
		}
		return mapper.update(this, attributes, values, criteria, currentUser);
	}

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The list of attributes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @return true if the item has been successfully updated.
	 */
	public boolean dataUpdate(int itemId, List<MetaDataAttribute> attributes, List<Object> values) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		if ((attributes == null) || attributes.isEmpty()) {
			return mapper.update(this, itemId, null);
		}
		return mapper.update(this, itemId, attributes, values);
	}

	/**
	 * Update an item into the storage.
	 * 
	 * <p>
	 * Update operation can be called onto (soft) deleted items.
	 * 
	 * @param itemId
	 *            The item ID to update.
	 * @param attributes
	 *            The list of attributes to be updated.
	 * @param values
	 *            The corresponding list of values. This list length must be the same as the attribute list length.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the item has been successfully updated.
	 */
	public boolean dataUpdate(int itemId, List<MetaDataAttribute> attributes, List<Object> values, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		if ((attributes == null) || attributes.isEmpty()) {
			return mapper.update(this, itemId, null, currentUser);
		}
		return mapper.update(this, itemId, attributes, values, currentUser);
	}
	
	/**
	 * Count the number of items of the specified entity. Deleted items are ignored.
	 * 
	 * @return the number of items selected.
	 */
	public int dataCount() {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCount,getType()));
		}
		return mapper.count(this);
	}

	/**
	 * Count the number of selected items that possess the given attribute value.
	 * 
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the number of items selected.
	 */
	public int dataCount(boolean deleted, String attribute, Object value) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCount,getType()));
		}
		return mapper.count(this, deleted, getAttributeLine(attribute), value);
	}

	/**
	 * Count the number of selected items that correspond to the given condition.
	 * 
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be counted (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the number of items selected.
	 */
	public int dataCount(boolean deleted, ISearchCriteria criteria, boolean distinct, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataCount,getType()));
		}
		return mapper.count(this, deleted, criteria, distinct, currentUser);
	}

	/**
	 * Test a complex condition and return true if it succeed.
	 * 
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the selection return at least one item.
	 */
	public boolean dataTest(ISearchCriteria criteria, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, criteria, currentUser);
	}

	/**
	 * Test a complex condition against a specific item and return true if it succeed.
	 * 
	 * @param itemId
	 *            The item ID to test.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the condition is applicable to the given item.
	 */
	public boolean dataTest(int itemId, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, itemId, criteria, currentUser);
	}

	/**
	 * Utility method user to test that the given user possess the required right to access the entity.
	 * 
	 * <p>
	 * If the entity access right are simple this method do not access the data source (server or database).
	 * But if required it may do so (and have performance draw back).
	 * In some circumstances it may be more efficient to directly try the operation end catch a posible 
	 * access error. 
	 *   
	 * @param currentUser
	 * @return
	 */
	public boolean dataHasRightCreate(IConnectionUserBean currentUser) {
		if (currentUser == null) {
			return false;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, getRightCreate(), currentUser);
	}

	/**
	 * Utility method user to test that the given user possess the required right to access the entity.
	 * 
	 * <p>
	 * If the entity access right are simple this method do not access the data source (server or database).
	 * But if required it may do so (and have performance draw back).
	 * In some circumstances it may be more efficient to directly try the operation end catch a posible 
	 * access error. 
	 *
	 * @param itemId a valid item Id from this entity.
	 * @param currentUser
	 * @return
	 */
	public boolean dataHasRightRead(int itemId, IConnectionUserBean currentUser) {
		if (currentUser == null) {
			return false;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, itemId, getRightRead(), currentUser);
	}

	/**
	 * Utility method user to test that the given user possess the required right to access the entity.
	 * 
	 * <p>
	 * If the entity access right are simple this method do not access the data source (server or database).
	 * But if required it may do so (and have performance draw back).
	 * In some circumstances it may be more efficient to directly try the operation end catch a posible 
	 * access error. 
	 *
	 * @param itemId a valid item Id from this entity.
	 * @param currentUser
	 * @return
	 */
	public boolean dataHasRightUpdate(int itemId, IConnectionUserBean currentUser) {
		if (currentUser == null) {
			return false;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, itemId, getRightUpdate(), currentUser);
	}

	/**
	 * Utility method user to test that the given user possess the required right to access the entity.
	 * 
	 * <p>
	 * If the entity access right are simple this method do not access the data source (server or database).
	 * But if required it may do so (and have performance draw back).
	 * In some circumstances it may be more efficient to directly try the operation end catch a posible 
	 * access error. 
	 *
	 * @param itemId a valid item Id from this entity.
	 * @param currentUser
	 * @return
	 */
	public boolean dataHasRightDelete(int itemId, IConnectionUserBean currentUser) {
		if (currentUser == null) {
			return false;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, itemId, getRightDelete(), currentUser);
	}

	/**
	 * Utility method user to test that the given user possess the required right to access the entity.
	 * 
	 * <p>
	 * If the entity access right are simple this method do not access the data source (server or database).
	 * But if required it may do so (and have performance draw back).
	 * In some circumstances it may be more efficient to directly try the operation end catch a posible 
	 * access error. 
	 *
	 * @param currentUser
	 * @return
	 */
	public boolean dataHasRightList(IConnectionUserBean currentUser) {
		if (currentUser == null) {
			return false;
		}
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataTest,getType()));
		}
		return mapper.test(this, getRightList(), currentUser);
	}

	/**
	 * Create a link between two data according to a predefined link.
	 * 
	 * @param source The origin of the reference.
	 * @param linkCode The link code (must be defined into this entity).
	 * @param dest The destination object of the link.
	 * @return true if the operation is a success.
	 */
	public boolean dataLinkTo(BeanMap source, String linkCode, BeanMap dest) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkAdd(source, linkCode, dest);
	}

	/**
	 * Create a link between two data according to a predefined link.
	 * 
	 * @param source The origin of the reference.
	 * @param linkCode The link code (must be defined into this entity).
	 * @param dest The destination object of the link.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the operation is a success.
	 */
	public boolean dataLinkTo(BeanMap source, String linkCode, BeanMap dest, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkAdd(source, linkCode, dest, currentUser);
	}
	
	/**
	 * Remove a link between two data according to a predefined link.
	 * 
	 * @param source The origin of the reference.
	 * @param linkCode The link code (must be defined into this entity).
	 * @param dest The destination object of the link.
	 * @return true if the operation is a success.
	 */
	public boolean dataLinkRemove(BeanMap source, String linkCode, BeanMap dest) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkRemove(source.getType(), linkCode, source.getId(), dest.getId());
	}	
	
	/**
	 * Remove a link between two data according to a predefined link.
	 * 
	 * @param source The origin of the reference.
	 * @param linkCode The link code (must be defined into this entity).
	 * @param dest The destination object of the link.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return true if the operation is a success.
	 */
	public boolean dataLinkRemove(BeanMap source, String linkCode, BeanMap dest, IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataUpdate,getType()));
		}
		return mapper.linkRemove(source.getType(), linkCode, source.getId(), dest.getId());
	}	
		
	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the first result of the selection.
	 */
	public BeanMap dataSelectionFirst(String attributes, boolean deleted, String attributeTest, Object value) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.selectionFirst(this, attributes, deleted, attributeTest, value);
	}
	
	/**
	 * Return the first result of the selection.
	 * 
	 * <p>
	 * This method can be faster than calling the list selection with a page length of one item (
	 * {@link #selection(String, String, boolean, String, Object)}).
	 * 
	 * @param attributes
	 *            The list of attributes to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition to be tested.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return the first result of the selection.
	 */
	public BeanMap dataSelectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			IConnectionUserBean currentUser) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.selectionFirst(this, attributes, deleted, criteria, currentUser);
	}

	/**
	 * Select an item from the storage.
	 * 
	 * @param itemId
	 *            the item ID to select.
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the item can be deleted.
	 * @return the selected item, or null if does not exist or is deleted (and <code>deleted</code> paramter is false).
	 */
	public BeanMap dataSelection(int itemId, String attributes, boolean deleted) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		if (attributes == null) {
			return mapper.selection(this, itemId, getAllAttributes(), deleted);
		}
		return mapper.selection(this, itemId, getAttributeLines(attributes), deleted);
	}

	/**
	 * Select all the items of a given entity.
	 * 
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList dataSelection(String attributes) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.selection(this, attributes);
	}
	
	/**
	 * Select all the items of a given entity.
	 * 
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList dataSelection() {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.selection(this);
	}
	
	/**
	 * Select the items that possess the given attribute value.
	 * 
	 * @param attributes
	 *            The spaces separated, list of attributes codes to select. If null, all of the attributes values will
	 *            be returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param attributeTest
	 *            The attribute code to test.
	 * @param value
	 *            The attribute value to test.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList dataSelection(String attributes, boolean deleted, String attributeTest, Object value) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		return mapper.selection(this, attributes, deleted, attributeTest, value);
	}

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * @param attributes
	 *            The list of attributes to select. If this parameter is null all listables attributes will be returned. If the list is empty none attributes are returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The list of attributes used to sort the result. Theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, or an empty list if nothing is selected.
	 */
	public BeanMapList dataSelection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser, int page, int limit) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		if (attributes == null) {
			return mapper.selection(this, getListables(), deleted, criteria, distinct, orders, currentUser, page, limit);
		}
		if (attributes.size() == 0) {
			return mapper.selection(this, deleted, criteria, distinct, currentUser, page, limit);
		}
		return mapper.selection(this, attributes, deleted, criteria, distinct, orders, currentUser, page, limit);
	}

	/**
	 * Select some items form an entity type according to a complex selection condition.
	 * 
	 * <p>
	 * Note that if the page and limit parameter are defined, the result is a BeanMapPartialList.
	 * 
	 * @param attributes
	 *            The list of attributes to select. If this parameter is null all listables attributes will be returned. If the list is empty none attributes are returned.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria
	 *            The complex condition for the selection.
	 * @param distinct
	 *            if true then only distinct items will be returned (not duplicated results). This option is not
	 *            supported by all mappers.
	 * @param orders
	 *            The list of attributes used to sort the result. Theses attributes need to be present into the selected
	 *            attributes list. To process to a inverted sort flag the ReferenceLine.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return the list of the selected items, which may be a BeanMapPartialList, or an empty list if nothing is selected.
	 */
	public BeanMapList dataSelection(String attributes, boolean deleted, String criteria,
			boolean distinct, String orders, IConnectionUserBean currentUser, int page, int limit) {
		IMapperService mapper = getMapper();
		if (mapper == null) {
			throw new ResourceException(Status.SERVER_ERROR_INTERNAL, String.format(Messages.MetaDataEntity_Error_DataSelect,getType()));
		}
		BeanMapList result;
		if (attributes == null) {
			result = mapper.selection(this, getListables(), deleted, getCriteria(criteria), distinct, getAttributeLines(orders), currentUser, page, limit);
		} else if (attributes.length() == 0) {
			result = mapper.selection(this, deleted, getCriteria(criteria), distinct, currentUser, page, limit);
		} else {
			result = mapper.selection(this, getAttributeLines(attributes), deleted, getCriteria(criteria), distinct, getAttributeLines(orders), currentUser, page, limit);
		}
		// On the server side "mapper.selection" do not initialize the "total" property of the return list.
		if ((result instanceof BeanMapPartialList) && (((BeanMapPartialList) result).getTotal() == 0) && (result.size() > 0)) {
			((BeanMapPartialList) result).setTotal(dataCount(deleted, create, distinct, currentUser));
		}
		return result;
	}
	
	/**
	 * Return a selection from the given list of BeanMap. 
	 * 
	 * <p>
	 * Note that this method is a "local" selection, it does not call a data source, nor a web-service. 
	 * All the required attributes must be into the list.
	 * 
	 * @param list a BeanMap list that may contain the item to select.
	 * @param criteria The complex condition for the selection, may be null.
	 * @param user
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return a non null list of selected items from within the <code>list</code> parameter.
	 */
	public BeanMapList dataSelection(BeanMapList list, ISearchCriteria criteria, IConnectionUserBean user) {
		if (criteria instanceof AbstractSearchCriteria) {
			return ((AbstractSearchCriteria) criteria).select(list, user);
		}
		return (BeanMapList) list.clone();
	}
	
	/**
	 * Return a selection from the given list of BeanMap. 
	 * 
	 * <p>
	 * Note that this method is a "local" selection, it does not call a data source, nor a web-service. 
	 * All the required attributes must be into the list.
	 * 
	 * <p>
	 * The result of this is a BeanMapPartialList if the page and limit parameter are defined. In that
	 * case the <b>rank</b> and <b>total</b> properties of the list are initialized with the original list of values.
	 *  
	 * @param list a BeanMap list that may contain the item to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria The complex condition for the selection, may be null.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @param page
	 *            The number of the first result row to return, zero for first row.
	 * @param limit
	 *            The maximal number of row to return, -1 if unlimited.
	 * @return a non null list of selected items from within the <code>list</code> parameter.
	 */
	public BeanMapList dataSelection(BeanMapList list, boolean deleted, ISearchCriteria criteria, IConnectionUserBean user, int page, int limit) {
		BeanMapList result = dataSelection(list, criteria, user);
		if (!deleted) {
			Iterator<BeanMap> itt = result.iterator();
			while(itt.hasNext()) {
				if (itt.next().isDeleted()) {
					itt.remove();
				}
			}
		}
		if ((page > 0) || (limit > 0)) {
			if (page >= result.size()) {
				result = new BeanMapPartialList();
			} else {
				int l = page + limit;
				if (l >= result.size()) {
					result = new BeanMapPartialList(result.subList(page, result.size()));
				} else {
					result = new BeanMapPartialList(result.subList(page, l));
				}
			}
			((BeanMapPartialList) result).setTotal(list.size());
			((BeanMapPartialList) result).setRank(page);
		}
		return result;
	}
	
	/**
	 * Return a selection from the given list of BeanMap. 
	 * 
	 * <p>
	 * Note that this method is a "local" selection, it does not call a data source, nor a web-service. 
	 * All the required attributes must be into the list.
	 * 
	 * @param list a BeanMap list that may contain the item to select.
	 * @param deleted
	 *            True if the selected items can be deleted. If False, then only not deleted items will be listed.
	 * @param criteria The complex condition for the selection, may be null.
	 * @param currentUser
	 *            The connected user that is at the origin of this request. Can be null.
	 * @return null or the first item selected from within the <code>list</code> parameter.
	 */
	public BeanMap dataSelectionFirst(BeanMapList list, ISearchCriteria criteria, boolean deleted, IConnectionUserBean user) {
		if (criteria instanceof AbstractSearchCriteria) {
			for (BeanMap b: list) {
				if (((AbstractSearchCriteria) criteria).test(b, user) && (deleted || !b.isDeleted())) {
					return b;
				}
			}
		} else if (!deleted) {
			for (BeanMap b: list) {
				if (!b.isDeleted()) {
					return b;
				}
			}
		}
		return null;
	}
	
	/**
	 * Check that this entity is available into the data container associated.
	 * 
	 * <p>
	 * This may log errors messages if this entity is not correctly configured. The reasons why this method may return false are:
	 * 
	 * <ul>
	 * <li>No mapper is (no more) associated to this entity. 
	 * <li>The datasource is not connected to the database, or any data container associated with.
	 * <li>the Table does not exist into the database, on more generally the data container is not ready to store this entity.
	 * </ul>
	 * 
	 * <p>
	 * This method execute a call to the data-container and may teake some time to responds, do not use to avoid a call to 
	 * the server or the database, this is often more efficient to just execute a data* method and catch the error.
	 * 
	 * @return true only if this entity actually exists into the container.
	 */
	public boolean exists() {
		try {
			dataCount();
			return true;
		} catch (ResourceException e) {
			return false;
		}
	}
	
	/**
	 * Add a new Attribute to this entity.
	 * 
	 * @param attribute
	 */
	public void addAttribute(MetaDataAttribute attribute) {
		attribute.setParent(this);
		attributes.put(attribute.getCode(), attribute);
	}

	/**
	 * Add a new link to this entity.
	 * 
	 * @param link
	 */
	public void addLink(MetaDataLink link) {
		link.setParent(this);
		links.put(link.getCode(), link);
	}

	/**
	 * Add a new Test to this entity.
	 * 
	 * @param test
	 */
	public void addTest(MetaDataTest test) {
		test.setParent(this);
		tests.put(test.getCode(), test);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(type);
		sb.append(' ');
		sb.append(attributes.keySet());
		return sb.toString();
	}

	/**
	 * Get le lists of codes used by Links and Attributes of this entity.
	 * 
	 * @return
	 */
	public Set<String> getCodes() {
		HashSet<String> result = new HashSet<String>();
		result.addAll(links.keySet());
		result.addAll(attributes.keySet());
		return result;
	}

	/**
	 * Return true if this entity define at least one link targeting itself.
	 * @return
	 */
	public boolean hasRecursiveLink() {
		for (MetaDataLink link: links.values()) {
			if (link.isRecursive()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Get the first link defined in this Entity and targeting the same one.
	 * 
	 * <p>
	 * If this entity use the "norecursive" flag MetaData the this method will return null.
	 * 
	 * @return null if there is no recursive link.
	 */
	public MetaDataLink getFirstRecursiveLink() {
		if (!getMetadata().getBoolean(MetaDataEntity.METADATA_IGNORERECURSIVITY)) {
			for (MetaDataLink link: links.values()) {
				if (link.isRecursive()) {
					return link;
				}
			}
		}
		return null;
	}
	
	/**
	 * Get a list of link according to the given chain of code. 
	 * 
	 * <p>
	 * The returned links are all atomic link (not combined with the "combo" metadata).
	 * This method can be used to resolve a single combined link by passing a single code as parameter.
	 *  
	 * @param codes a list of chained link codes.
	 * @return null if one of the code does not exist or if the chain is broken.
	 */
	public List<MetaDataLink> getLinkChain(String... codes) {
		if ((codes == null) || (codes.length == 0)) {
			return null;
		}
		List<MetaDataLink> result = new ArrayList<>(codes.length);
		getLinkChain(result, codes);
		if (result.isEmpty()) {
			return null;
		}
		return result;
	}
	
	/**
	 * Get a list of link according to the given chain of code. 
	 * 
	 * <p>
	 * The returned links are all atomic link (not combined with the "combo" metadata).
	 * This method can be used to resolve a single combined link by passing a single code as parameter.
	 *  
	 * @param links the resulting list 
	 * @param codes a list of chained link codes.
	 * @return null if one of the code does not exist or if the chain is broken.
	 */
	public void getLinkChain(List<MetaDataLink> links, String... codes) {
		MetaDataEntity e = this;
		for (String code: codes) {
			if ((code != null) && !code.isEmpty()) {
				MetaDataLink l = e.getLink(code);
				if (l == null) {
					return;
				}
				l.addAtomicLinks(links);
				if (!links.isEmpty()) {
					e = links.get(links.size() - 1).getRefEntity();
				}
			}
		}
		
	}
}
