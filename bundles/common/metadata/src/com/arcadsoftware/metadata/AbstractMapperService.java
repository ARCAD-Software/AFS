/*******************************************************************************
 * Copyright (c) 2024 ARCAD Software.
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
import java.util.Collections;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.restlet.data.Status;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.CriteriaContextBasic;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.IAttributeCriteria;
import com.arcadsoftware.metadata.criteria.IAttributesCriteria;
import com.arcadsoftware.metadata.criteria.ICriteriaContext;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.InGroupCriteria;
import com.arcadsoftware.metadata.criteria.IsNullCriteria;
import com.arcadsoftware.metadata.criteria.IsTrueCriteria;
import com.arcadsoftware.metadata.criteria.LinkEqualCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.internal.Activator;
import com.arcadsoftware.metadata.internal.Messages;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;
import com.arcadsoftware.osgi.ISODateFormater;
import com.arcadsoftware.rest.connection.IConnectionUserBean;

/**
 * This implementation provide default implementation of all trivial operations to minimize the number of operation to
 * implements.
 * 
 * <p>
 * Theses trivial operations are :
 * <ul>
 * <li>Transforms "type" string into entities. And test entity existence.
 * <li>Build list of ReferenceLine or MetaDataAttributes.
 * <li>Reduce and test trivial Criterion.
 * <li>Selection with default (null) list of attributes.
 * <li>Processing of reversed link (transforms theses link operation into operation onto target entity, and its mapper).
 * <li>Ensure that empty or null ISearchCriteria are associated to a TRUE condition.
 * </ul>
 * 
 * <p>
 * This class also propose some useful tools for extender classes.
 * 
 */
public abstract class AbstractMapperService implements IMapperService {

	/*
	 * Limite au dela de laquelle on ne génère plus une sous conditions à partir d'une sélection extra domaine
	 */
	private static final int EXTRADOMAINCONDITION_MAXIMUM = 20;

	/**
	 * Define the mapper properties.
	 * 
	 * <p>
	 * Most of theses properties are informational.
	 * 
	 * @param domainMame
	 *            the Mapper domain name.
	 * @param softdeletion
	 *            true if this Mapper support the soft deletion feature.
	 * @param pagination
	 *            true if this Mapper support natively the pagination of selection feature.
	 * @param multilink
	 *            true if this Mapper support the multiple links references feature.
	 * @param extrarefs
	 *            true if this Mapper support the references to other Mappers feature.
	 * @param groups
	 *            true if this Mapper support the entities groups feature.
	 * @return the OSGi service properties.
	 * @see IMapperService#PROP_DOMAINNAME
	 * @see IMapperService#PROP_SUPPORT_EXTRAREFERENCES
	 * @see IMapperService#PROP_SUPPORT_GROUPSENTITY
	 * @see IMapperService#PROP_SUPPORT_MULTILINKREFERENCES
	 * @see IMapperService#PROP_SUPPORT_PAGINATION
	 * @see IMapperService#PROP_SUPPORT_SOFTDELETION
	 */
	public static Dictionary<String, ?> mapperProperties(String domainMame, boolean softdeletion, boolean pagination,
			boolean multilink, boolean extrarefs, boolean groups) {
		Hashtable<String, Object> props = new Hashtable<String, Object>();
		props.put(PROP_DOMAINNAME, domainMame);
		props.put(PROP_SUPPORT_SOFTDELETION, softdeletion);
		props.put(PROP_SUPPORT_PAGINATION, pagination);
		props.put(PROP_SUPPORT_MULTILINKREFERENCES, multilink);
		props.put(PROP_SUPPORT_EXTRAREFERENCES, extrarefs);
		props.put(PROP_SUPPORT_GROUPSENTITY, groups);
		return props;
	}

	private final XmlCriteriaStream xsCriteria;
	private final ArrayList<String> domains;

	public AbstractMapperService() {
		super();
		xsCriteria = new XmlCriteriaStream();
		domains = new ArrayList<String>();
	}

	@Override
	public void addDomain(String domain) {
		domains.add(domain);
	}

	@Override
	public boolean sameDomain(MetaDataEntity entity) {
		for (String domain: domains) {
			if (domain.equalsIgnoreCase(entity.getDomain())) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public boolean sameDomain(String domain) {
		for (String d: domains) {
			if (d.equalsIgnoreCase(domain)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Return true if this element (MetaDataAttribute) possess an 
	 * Encryption meda data that must be handled by the mapper. 
	 *  
	 * @param element
	 * @return
	 */
	protected boolean isEncrypted(Element element) {
		String s = element.getMetadata().getString(MetaDataEntity.METADATA_CRYPT);
		return (s != null) && //
				("mapper".equalsIgnoreCase(s) || //$NON-NLS-1$
						"database".equalsIgnoreCase(s) || //$NON-NLS-1$
						"storage".equalsIgnoreCase(s)); //$NON-NLS-1$
	}

	/**
	 * Facility method return the entity associated to the given type.
	 * 
	 * <p>
	 * This implementation never return null.
	 * 
	 * @return can return null if the OSGi Service is not implemented.
	 * @throws MapperException
	 *             if the bundle is not started or non registry implementation is ready.
	 */
	protected MetaDataEntity getEntity(String type) {
		Activator activator = Activator.getInstance();
		if (activator == null) {
			throw new MapperException(Status.SERVER_ERROR_SERVICE_UNAVAILABLE,
					Messages.AbstractMapperService_Error_InitializationFailed);
		}
		return activator.getEntity(type);
	}

	/**
	 * This method transform an XML string representation of a criteria into and Java Object.
	 * 
	 * @param criteria
	 * @return Criteria True is the given string is not a criteria representation.
	 */
	protected ISearchCriteria getCriteria(String criteria) {
		if ((criteria != null) && !criteria.isEmpty()) {
			Object c = xsCriteria.fromXML(criteria);
			if (c instanceof ISearchCriteria) {
				return (ISearchCriteria) c;
			}
		}
		return ConstantCriteria.TRUE;
	}

	/**
	 * Get a list of "T".
	 * 
	 * @param <T>
	 *            The type of the result.
	 * @param o
	 *            A set of objects.
	 * @return
	 */
	@SafeVarargs
	public static final <T> List<T> list(T... o) {
		ArrayList<T> list = new ArrayList<T>();
		Collections.addAll(list, o);
		return list;
	}

	/**
	 * Get the list of selected objects.
	 * 
	 * @param entity
	 * @param attributes
	 * @return
	 */
	protected List<ReferenceLine> getAttributesList(MetaDataEntity entity, String attributes) {
		if (attributes == null) {
			return entity.getListables();
		}
		return entity.getAttributeLines(attributes);
	}

	/**
	 * Get the list of selected objects.
	 * 
	 * @param entity
	 * @param attributes
	 * @return
	 */
	protected List<ReferenceLine> getAllAttributesList(MetaDataEntity entity, String attributes) {
		if (attributes == null) {
			return entity.getAllAttributes();
		}
		return entity.getAttributeLines(attributes);
	}

	/**
	 * Filter the BenaMap columns.
	 * <p>
	 * Return a new instance of the BeanMap object.
	 * 
	 * @param bean
	 * @param attributes
	 * @return
	 */
	protected BeanMap filterBean(BeanMap bean, List<ReferenceLine> attributes) {
		BeanMap result = bean.duplicate();
		if (attributes != null) {
			for (ReferenceLine ref : attributes) {
				result.put(ref.getCode(), bean.get(ref.getCode()));
			}
		}
		return result;
	}

	private ISearchCriteria getTestCriteria(ReferenceLine attributeTest, Object value, ICriteriaContext context) {
		context.useReference(attributeTest);
		if (value == null) {
			return new IsNullCriteria(attributeTest.getCode());
		}
		if (value instanceof Integer) {
			return new EqualCriteria(attributeTest.getCode(), (Integer) value);
		}
		if (value instanceof Boolean) {
			if ((Boolean) value) {
				return new IsTrueCriteria(attributeTest.getCode());
			}
			return new NotCriteria(new IsTrueCriteria(attributeTest.getCode()));
		}
		if (value instanceof Date) {
			return new EqualCriteria(attributeTest.getCode(), ISODateFormater.toString((Date) value));
		}
		return new EqualCriteria(attributeTest.getCode(), value.toString());
	}

	/**
	 * This method is called to build Criteria context just before criteria reduction.
	 * 
	 * <p>
	 * Implementor can override this method to get their own context Object.
	 * 
	 * @param entity
	 * @param currentUser
	 * @return
	 */
	protected ICriteriaContext getContext(MetaDataEntity entity, IConnectionUserBean currentUser) {
		return new CriteriaContextBasic(entity, currentUser);
	}

	/**
	 * This method process to selection of foreign attributes (references line that goes to other domains).
	 * 
	 * @param attributes
	 * @param result
	 * @return
	 */
	protected BeanMapList completeForeignAttributes(List<ReferenceLine> attributes, BeanMapList result) {
		// Cette implémentation minimize le nombre d'appels aux sous sélections (a priori les
		// opérations les plus couteuses). Elle n'en reste pas moins couteuse en CPU et en utilisation mémoire.
		if (attributes.size() == 0) {
			return result;
		}
		Map<ReferenceLine, List<ReferenceLine>> refs = ReferenceLine.fillDomainReferences(attributes);
		if (refs == null) {
			return result;
		}
		if (refs.size() > 4) {
			Activator.getInstance().info(
					String.format(Messages.AbstractMapperService_Info_ComplexSelection, refs.size()));
		}
		for (Entry<ReferenceLine, List<ReferenceLine>> e : refs.entrySet()) {
			HashMap<Integer, BeanMap> cache = new HashMap<Integer, BeanMap>();
			for (BeanMap bean : result) {
				Integer id = bean.get(e.getKey().getCode(), Integer.class);
				if (id != null) {
					BeanMap ref = cache.get(id);
					if (ref == null) {
						MetaDataEntity entity = e.getKey().getLastAttribute().getRefEntity();
						if (entity != null) {
							ref = entity.getMapper().selection(entity, id, e.getValue(), true);
						}
						if (ref == null) {
							ref = new BeanMap();
						}
						cache.put(id, ref);
					}
					bean.addAll(e.getKey().getCode() + '.', ref);
				}
			}
		}
		return result;
	}

	/**
	 * This method process to selection of foreign attributes (references line that goes to other domains).
	 * 
	 * @param attributes
	 * @param result
	 * @return
	 */
	protected BeanMap completeForeignAttributes(List<ReferenceLine> attributes, BeanMap result) {
		// Cette implémentation minimize le nombre d'appels aux sous sélections (a priori les
		// opérations les plus couteuses). Elle n'en reste pas moins couteuse en CPU et en utilisation mémoire.
		if ((attributes.size() == 0) || (result == null)) {
			return result;
		}
		Map<ReferenceLine, List<ReferenceLine>> refs = ReferenceLine.fillDomainReferences(attributes);
		if (refs == null) {
			return result;
		}
		if (refs.size() > 4) {
			Activator.getInstance().info(
					String.format(Messages.AbstractMapperService_Info_ComplexSelection, refs.size()));
		}
		for (Entry<ReferenceLine, List<ReferenceLine>> e : refs.entrySet()) {
			Integer id = result.get(e.getKey().getCode(), Integer.class);
			if (id != null) {
				MetaDataEntity entity = e.getKey().getLastAttribute().getRefEntity();
				if (entity != null) {
					result.addAll(e.getKey().getCode() + '.',
							entity.getMapper().selection(entity, id, e.getValue(), true));
				}
			}
		}
		return result;
	}

	/**
	 * This method transform the criteria to a domain local criteria. It replace the foreign part of the criteria with
	 * pre-selected datas.
	 * 
	 * <p>
	 * This implementation is limited to few sub-selection.
	 * 
	 * <p>
	 * The context is updated to be usable with the new Criteria. By the way its remains usable with the old criteria,
	 * who may be the same.
	 * 
	 * <p>
	 * This method may return a ConstantCriteria that should be tested to quickly resolve the
	 * 
	 * @param criteria
	 *            The criteria to make local. It need to be reduced first.
	 * @param context
	 *            The associated criteria context.
	 * @return a new criteria where all condition use the local domain of the referenced entity.
	 */
	protected ISearchCriteria completeForeignCriteria(ISearchCriteria criteria, ICriteriaContext context) {
		// Note:
		// Le principe de cette implémentation repose sur la décomposition de la condition
		// en sous conditions locales à chaque référence externe.
		// Par exemple (si "a1" à "a9" sont des attribut du domaine "a" etc pour "b" et "c") :
		// (a1.a2.b1 = 1) et ((a3.c2 = 2) ou (a4.b2 = 3))
		// peut être décomposé en :
		// Sélection sur le domaine B des élément : (b1 = 1).
		// puis reconstruction de la condition (a1.a2 = res) où res est un élément sélectionné par
		// la condition précédente.
		// Sélection sur le domaine C : (c1 = 2) puis reconstruction de (a3 = res).
		// Sélection sur le domaine B (à nouveau) : (b2 = 3) reconstruction en (a4 = 3).
		// enfin on peut garantir l'exécution de la condition complète sur le domaine A :
		// (a1.a2 = resB1)... et ((a3 = resC)... ou (a4 = resB2)...)
		// Au risque d'obtenir une condition relativement complexe si les sous sélections sont trop
		// nombreuses. Pour limiter ce problème une exception est déclenchée si une sous sélection
		// dépasse le seuil fixé par la constante EXTRADOMAINCONDITION_MAXIMUM
		//
		// Cette implémentation est multi-récussive. C'est à dire qu'elle est récursive sur l'arbre de
		// la condition à localiser. Elle est aussi récursive dans le sens ou chaque sous condition
		// sera elle aussi localisée au sous domaine conserné. Ainsi chaque condition peut transiter
		// par des domaines intermédiaire.
		//
		// Bien sur ce traitement entraîne des performances dégradées dans le cas d'un usage massif
		// de requêtes multi-domaines.
		//
		Map<String, ReferenceLine> foreigners = ReferenceLine.extraDomainReferences(context.getReferences());
		if ((foreigners == null) || (foreigners.size() == 0)) {
			return criteria;
		}
		// Cette méthode travaille de manière récursive.
		if (criteria instanceof AndCriteria) {
			return convertForeignCriterion(((AndCriteria) criteria).getCriterias(), true, context, foreigners);
		}
		if (criteria instanceof OrCriteria) {
			return convertForeignCriterion(((OrCriteria) criteria).getCriterias(), false, context, foreigners);
		}
		if (criteria instanceof NotCriteria) {
			return new NotCriteria(completeForeignCriteria(((NotCriteria) criteria).getCriteria(), context));
		}
		if (criteria instanceof InGroupCriteria) {
			if ((foreigners.get(((InGroupCriteria) criteria).getGroup()) != null) || //
					(foreigners.get(((InGroupCriteria) criteria).getItem()) != null)) {
				throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED,
						Messages.AbstractMapperService_Error_NoMultidomainForInGroupCriteria);
			}
			return criteria;
		}
		if (criteria instanceof LinkEqualCriteria) {
			if ((foreigners.get(((LinkEqualCriteria) criteria).getReference()) != null) || //
					(foreigners.get(((LinkEqualCriteria) criteria).getAttribute()) != null)) {
				throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED,
						Messages.AbstractMapperService_Error_NoMultidomainForLinkEqualsCriteria);
			}
		}
		if (criteria instanceof IAttributeCriteria) {
			return convertForeignCriterion(list(criteria), false, context, foreigners);
		}
		return criteria;
	}

	/*
	 * Fonction auxiliaire (récursive) de la méthode completeForeignCriteria (ci-dessus).
	 */
	private ISearchCriteria convertForeignCriterion(List<ISearchCriteria> criterias, boolean andParent,
			ICriteriaContext context, Map<String, ReferenceLine> foreigners) {
		ArrayList<ISearchCriteria> result = new ArrayList<ISearchCriteria>();
		HashMap<ReferenceLine, List<ISearchCriteria>> fcriterion = new HashMap<ReferenceLine, List<ISearchCriteria>>();
		// 1. construction des conditions de sous-sélection.
		for (ISearchCriteria criteria : criterias) {
			if (criteria instanceof IAttributesCriteria) {
				ReferenceLine lref = foreigners.get(((IAttributesCriteria) criteria).getAttribute());
				ReferenceLine lsref = foreigners.get(((IAttributesCriteria) criteria).getSecondAttribute());
				if ((lref == null) && (lsref == null)) {
					result.add(criteria);
				} else {
					if ((lref == null) || !lref.equals(lsref)) {
						throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED,
								Messages.AbstractMapperService_Error_NoMultidomainForMultiAttributesCriteria);
						// Les tests incluant des références externes ne sont
						// supportées que si la branche de référence est la même.
						// par exemple : a1.a2.b1 = a1.a2.b2
						// peut être transformer en un test (b1 = b2) puis (a1.a2 = res)
						// Toute autre configuration n'est pas compatible avec cette implémentation.
					}
					List<ISearchCriteria> fc = fcriterion.get(lref);
					if (fc == null) {
						fc = new ArrayList<ISearchCriteria>();
						fcriterion.put(lref, fc);
					}
					try {
						criteria = (ISearchCriteria) criteria.clone();
					} catch (CloneNotSupportedException e) {
						Activator.getInstance().debug(e);
					}
					((IAttributesCriteria) criteria).setAttribute(((IAttributesCriteria) criteria).getAttribute()
							.substring(lref.getCode().length() + 2));
					((IAttributesCriteria) criteria).setSecondAttribute(((IAttributesCriteria) criteria)
							.getSecondAttribute().substring(lref.getCode().length() + 2));
					fc.add(criteria);
				}
			} else if (criteria instanceof IAttributeCriteria) {
				ReferenceLine lref = foreigners.get(((IAttributeCriteria) criteria).getAttribute());
				if (lref == null) {
					result.add(criteria);
				} else {
					List<ISearchCriteria> fc = fcriterion.get(lref);
					if (fc == null) {
						fc = new ArrayList<ISearchCriteria>();
						fcriterion.put(lref, fc);
					}
					try {
						criteria = (ISearchCriteria) criteria.clone();
					} catch (CloneNotSupportedException e) {
						Activator.getInstance().debug(e);
					}
					((IAttributeCriteria) criteria).setAttribute(((IAttributeCriteria) criteria).getAttribute()
							.substring(lref.getCode().length() + 2));
					fc.add(criteria);
				}
			} else {
				ISearchCriteria c = completeForeignCriteria(criteria, context);
				if (ConstantCriteria.FALSE.equals(c)) {
					if (andParent) {
						return c;
					}
				} else if (ConstantCriteria.TRUE.equals(c)) {
					if (!andParent) {
						return c;
					}
				} else {
					result.add(c);
				}
			}
		}
		// 2. Exécution des sous-sélections.
		for (Entry<ReferenceLine, List<ISearchCriteria>> e : fcriterion.entrySet()) {
			ISearchCriteria c;
			if (andParent) {
				c = new AndCriteria(e.getValue());
			} else {
				c = new OrCriteria(e.getValue());
			}
			MetaDataEntity entity = e.getKey().getLastEntity();
			BeanMapList list = entity.getMapper().selection(entity, false, c, true, context.getCurrentUser(), 0,
					EXTRADOMAINCONDITION_MAXIMUM + 1);
			if (list == null) {
				continue;
			}
			String code = e.getKey().getCode();
			if (list.size() > EXTRADOMAINCONDITION_MAXIMUM) {
				String message = String.format(Messages.AbstractMapperService_Error_TooLargeSelection, code);
				Activator.getInstance().debug(message);
				throw new ResourceException(Status.CLIENT_ERROR_BAD_REQUEST, message);
			}
			// 3. Ajout des résultats à la condition locale
			context.useReference(e.getKey());
			for (BeanMap bean : list) {
				result.add(new EqualCriteria(code, bean.getId()));
			}
		}
		if (result.size() == 0) {
			return ConstantCriteria.FALSE;
		}
		if (result.size() == 1) {
			return result.get(0);
		}
		if (result.size() > 10) {
			Activator.getInstance().debug(Messages.AbstractMapperService_Debug_ComplexCondition + result.size());
		}
		if (andParent) {
			return new AndCriteria(result);
		}
		return new OrCriteria(result);
	}

	public final BeanMap create(BeanMap item) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return null;
		}
		List<MetaDataAttribute> attlist = new ArrayList<MetaDataAttribute>();
		return create(entity, attlist, entity.getValues(item, attlist));
	}

	public final BeanMap create(String type, String attributes, List<Object> values) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return null;
		}
		return create(entity, entity.getAttributes(attributes), values);
	}

	public BeanMap create(MetaDataEntity entity, String attributes, Object... values) {
		return create(entity, entity.getAttributes(attributes), list(values));
	}

	public final BeanMap create(MetaDataEntity entity, String attributes, List<Object> values) {
		return create(entity, entity.getAttributes(attributes), values);
	}

	public final BeanMap create(MetaDataAttribute attribute, Object value) {
		if (attribute == null) {
			return null;
		}
		return create(attribute.getParent(), list(attribute), list(value));
	}

	public final BeanMap create(List<MetaDataAttribute> attributes, List<Object> values) {
		if ((attributes == null) || (attributes.size() == 0)) {
			return null;
		}
		return create(attributes.get(0).getParent(), attributes, values);
	}

	public final boolean delete(BeanMap item, boolean hardDelete) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return false;
		}
		return delete(entity, item.getId(), hardDelete);
	}

	public final boolean delete(String type, int itemId, boolean hardDelete) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return false;
		}
		return delete(entity, itemId, hardDelete);
	}

	public final boolean undelete(BeanMap item) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return false;
		}
		return undelete(entity, item.getId());
	}

	public final boolean undelete(String type, int itemId) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return false;
		}
		return undelete(entity, itemId);
	}

	public final boolean update(BeanMap item) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return false;
		}
		List<MetaDataAttribute> attributes = new ArrayList<MetaDataAttribute>();
		return update(entity, item.getId(), attributes, entity.getValues(item, attributes));
	}

	public final boolean update(String type, int itemId, String attributes, List<Object> values) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return false;
		}
		return update(entity, itemId, entity.getAttributes(attributes), values);
	}

	public final boolean update(MetaDataEntity entity, int itemId, String attributes, List<Object> values) {
		return update(entity, itemId, entity.getAttributes(attributes), values);
	}

	public boolean update(MetaDataEntity entity, int itemId, String attributes, Object... values) {
		return update(entity, itemId, entity.getAttributes(attributes), list(values));
	}

	public final boolean update(int itemId, MetaDataAttribute attribute, Object value) {
		if (attribute == null) {
			return false;
		}
		return update(attribute.getParent(), itemId, list(attribute), list(value));
	}

	public final boolean update(String type, String attributes, List<Object> values, String criteria) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return false;
		}
		return update(entity, entity.getAttributes(attributes), values, getCriteria(criteria));
	}

	public final boolean update(MetaDataEntity entity, String[] attributes, List<Object> values, String criteria) {
		return update(entity, entity.getAttributes(attributes), values, getCriteria(criteria));
	}

	public final boolean update(int itemId, List<MetaDataAttribute> attributes, List<Object> values) {
		if ((attributes == null) || (attributes.size() == 0)) {
			return false;
		}
		return update(attributes.get(0).getParent(), itemId, attributes, values);
	}

	public final BeanMap selectionFirst(String type, String attributes, boolean deleted, String attributeTest,
			Object value) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return null;
		}
		return selectionFirst(getAttributesList(entity, attributes), deleted, entity.getAttributeLine(attributeTest),
				value);
	}

	public final BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted,
			String attributeTest, Object value) {
		return selectionFirst(getAttributesList(entity, attributes), deleted, entity.getAttributeLine(attributeTest),
				value);
	}

	public final BeanMap selectionFirst(MetaDataEntity entity, String attributes, boolean deleted,
			ReferenceLine attributeTest, Object value) {
		return selectionFirst(getAttributesList(entity, attributes), deleted, attributeTest, value);
	}

	public final boolean test(BeanMap item, ISearchCriteria criteria, IConnectionUserBean user) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return false;
		}
		return test(entity, item.getId(), criteria, user);
	}

	public final int count(String type) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return 0;
		}
		return count(entity, false, ConstantCriteria.TRUE, false, null);
	}

	public final int count(MetaDataEntity entity) {
		return count(entity, false, ConstantCriteria.TRUE, false, null);
	}

	public final int count(String type, boolean deleted, String attributeTest, Object value) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return 0;
		}
		return count(entity, deleted, entity.getAttributeLine(attributeTest), value);
	}

	@Override
	public final int count(String type, boolean deleted, String criteria, boolean distinct, IConnectionUserBean currentUser) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return 0;
		}
		return count(entity, deleted, getCriteria(criteria), distinct, currentUser);
	}

	@Override
	public final BeanMap selection(BeanMap item, boolean deleted) {
		MetaDataEntity entity = getEntity(item.getType());
		if (entity == null) {
			return null;
		}
		return selection(entity, item.getId(), entity.getAllAttributes(), deleted);
	}

	@Override
	public final BeanMap selection(String type, int itemId, String attributes, boolean deleted) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return null;
		}
		return selection(entity, itemId, getAllAttributesList(entity, attributes), deleted);
	}

	@Override
	public BeanMap selection(MetaDataEntity entity, int itemId) {
		return selection(entity, itemId, entity.getAllAttributes(), false);
	}

	public final BeanMap selection(MetaDataEntity entity, int itemId, String attributes, boolean deleted) {
		return selection(entity, itemId, getAllAttributesList(entity, attributes), deleted);
	}

	public final BeanMapList selection(String type) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return new BeanMapList();
		}
		return selection(entity, entity.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList selection(MetaDataEntity entity) {
		return selection(entity, entity.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList selection(MetaDataEntity entity, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser,
			int page, int limit) {
		if (attributes == null) {
			attributes = entity.getListables();
		}
		CriteriaContextBasic context = new CriteriaContextBasic(entity, currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
		}
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return new BeanMapList();
		}
		return doSelection(attributes, deleted, criteria, distinct, orders, page, limit, context);
	}

	public final BeanMapList selection(MetaDataEntity entity, String attributes) {
		if (entity == null) {
			return new BeanMapList();
		}
		return doSelection(getAttributesList(entity, attributes), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), 0, -1, new CriteriaContextBasic(entity, null));
	}

	public final BeanMapList selection(String type, String attributes, boolean deleted, String attributeTest,
			Object value) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return new BeanMapList();
		}
		return selection(getAttributesList(entity, attributes), deleted, entity.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted, String attributeTest,
			Object value) {
		return selection(getAttributesList(entity, attributes), deleted, entity.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList selection(String type, String attributes, boolean deleted, String criteria,
			boolean distinct, String orders, IConnectionUserBean currentUser, int page, int limit) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return new BeanMapList();
		}
		CriteriaContextBasic context = new CriteriaContextBasic(entity, currentUser);
		ISearchCriteria ctr = getCriteria(criteria).reduce(context);
		if (ConstantCriteria.FALSE.equals(ctr)) {
			return new BeanMapList();
		}
		return doSelection(getAttributesList(entity, attributes), deleted, ctr, distinct,
				entity.getAttributeLines(orders), page, limit, context);
	}

	public final BeanMapList selection(MetaDataEntity entity, String attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, String orders, IConnectionUserBean currentUser, int page,
			int limit) {
		CriteriaContextBasic context = new CriteriaContextBasic(entity, currentUser);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return new BeanMapList();
		}
		return doSelection(getAttributesList(entity, attributes), deleted, criteria, distinct,
				entity.getAttributeLines(orders), page, limit, context);
	}

	public final boolean linkAdd(BeanMap source, String linkCode, int destId) {
		MetaDataEntity entity = getEntity(source.getType());
		if (entity == null) {
			return false;
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return false;
		}
		return linkAdd(link, source.getId(), destId);
	}

	public final boolean linkAdd(BeanMap source, String linkCode, BeanMap dest) {
		MetaDataEntity entity = getEntity(source.getType());
		if (entity == null) {
			return false;
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return false;
		}
		return linkAdd(link, source.getId(), dest.getId());
	}

	public final boolean linkAdd(String sourceType, String linkCode, int sourceId, int destId) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return false;
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return false;
		}
		return linkAdd(link, sourceId, destId);
	}

	public final boolean linkAdd(MetaDataLink link, int sourceId, int destId) {
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			return doLinkAdd(link, sourceId, destId);
		}
		MetaDataEntity e = link.getRefEntity();
		if (e == null) {
			return false;
		}
		MetaDataAttribute att = e.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return false;
		}
		return e.getMapper().update(e, destId, list(att), list((Object) new Integer(sourceId)));
	}

	/**
	 * Process of link creation.
	 * 
	 * <p>
	 * Resersed references are <b>not</b> proceeded by this method.
	 * 
	 * <p>
	 * But "autolink" must be taken into account. Autolink are link automatically created from inversed links the value
	 * of this Metadata tag is the corresponding link code into the target entity.
	 * 
	 * @param link
	 *            the link metadate object.
	 * @param sourceId
	 * @param destId
	 * @return true
	 */
	protected abstract boolean doLinkAdd(MetaDataLink link, int sourceId, int destId);

	public final boolean linkTest(String sourceType, String linkCode, int sourceId, int destId) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return false;
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return false;
		}
		return linkTest(link, sourceId, destId);
	}

	public final boolean linkTest(MetaDataLink link, int sourceId, int destId) {
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			return doLinkTest(link, sourceId, destId);
		}
		MetaDataEntity e = link.getRefEntity();
		if (e == null) {
			return false;
		}
		MetaDataAttribute att = e.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return false;
		}
		return e.getMapper().test(e, destId, new EqualCriteria(att.getCode(), sourceId), null);
	}

	/**
	 * Perform a link test.
	 * 
	 * <p>
	 * Resersed references are <b>not</b> proceeded by this method.
	 * 
	 * <p>
	 * But "autolink" must be taken into account. Autolink are link automatically created from inversed links the value
	 * of this Metadata tag is the corresponding link code into the target entity.
	 * 
	 * @param link
	 * @param sourceId
	 * @param destId
	 * @return
	 */
	protected abstract boolean doLinkTest(MetaDataLink link, int sourceId, int destId);

	public final boolean linkRemove(String sourceType, String linkCode, int sourceId, int destId) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return false;
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return false;
		}
		return linkRemove(link, sourceId, destId);
	}

	public final boolean linkRemove(MetaDataLink link, int sourceId, int destId) {
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			return doLinkRemove(link, sourceId, destId);
		}
		MetaDataEntity e = link.getRefEntity();
		if (e == null) {
			return false;
		}
		MetaDataAttribute att = e.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return false;
		}
		return e.getMapper().update(e, destId, list(att), list((Object) null));
	}

	protected abstract boolean doLinkRemove(MetaDataLink link, int sourceId, int destId);

	public final BeanMapList linkSelection(String sourceType, String linkCode, int sourceId) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return new BeanMapList();
		}
		entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		return linkSelection(link, sourceId, entity.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList linkSelection(MetaDataLink link, int sourceId) {
		MetaDataEntity entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		return linkSelection(link, sourceId, entity.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String attributeTest, Object value) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return new BeanMapList();
		}
		entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		return linkSelection(link, sourceId, getAttributesList(entity, attributes), deleted,
				entity.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList linkSelection(MetaDataLink link, int sourceId, String attributes, boolean deleted,
			String attributeTest, Object value) {
		MetaDataEntity entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		return linkSelection(link, sourceId, getAttributesList(entity, attributes), deleted,
				entity.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList linkSelection(String sourceType, String linkCode, int sourceId, String attributes,
			boolean deleted, String criteria, boolean distinct, String orders, IConnectionUserBean currentUser,
			int page, int limit) {
		MetaDataEntity entity = getEntity(sourceType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataLink link = entity.getLink(linkCode);
		if (link == null) {
			return new BeanMapList();
		}
		entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		return linkSelection(link, sourceId, getAttributesList(entity, attributes), deleted, getCriteria(criteria),
				distinct, entity.getAttributeLines(orders), currentUser, page, limit);
	}

	public final boolean groupAdd(BeanMap group, int itemId) {
		MetaDataEntity entity = getEntity(group.getType());
		if (entity == null) {
			return false;
		}
		return groupAdd(entity, group.getId(), itemId);
	}

	public final boolean groupAdd(String groupType, int groupId, int itemId) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return false;
		}
		return groupAdd(entity, groupId, itemId);
	}

	public final boolean groupTest(BeanMap group, int itemId) {
		MetaDataEntity entity = getEntity(group.getType());
		if (entity == null) {
			return false;
		}
		return groupTest(entity, group.getId(), itemId);
	}

	public final boolean groupTest(String groupType, int groupId, int itemId) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return false;
		}
		return groupTest(entity, groupId, itemId);
	}

	public final boolean groupRemove(BeanMap group, int itemId) {
		MetaDataEntity entity = getEntity(group.getType());
		if (entity == null) {
			return false;
		}
		return groupRemove(entity, group.getId(), itemId);
	}

	public final boolean groupRemove(String groupType, int groupId, int itemId) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return false;
		}
		return groupRemove(entity, groupId, itemId);
	}

	public final BeanMapList groupSelection(BeanMap group) {
		MetaDataEntity entity = getEntity(group.getType());
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataEntity ge = getEntity(entity.getGroupType());
		if (ge == null) {
			return new BeanMapList();
		}
		return groupSelection(entity, group.getId(), ge.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList groupSelection(String groupType, int groupId) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataEntity ge = getEntity(entity.getGroupType());
		if (ge == null) {
			return new BeanMapList();
		}
		return groupSelection(entity, groupId, ge.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList groupSelection(MetaDataEntity groupEntity, int groupId) {
		MetaDataEntity ge = getEntity(groupEntity.getGroupType());
		if (ge == null) {
			return new BeanMapList();
		}
		return groupSelection(groupEntity, groupId, ge.getListables(), false, ConstantCriteria.TRUE, false,
				new ArrayList<ReferenceLine>(), null, 0, -1);
	}

	public final BeanMapList groupSelection(String groupType, int groupId, String attributes, boolean deleted,
			String attributeTest, Object value) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataEntity ge = getEntity(entity.getGroupType());
		if (ge == null) {
			return new BeanMapList();
		}
		List<ReferenceLine> attlist;
		if (attributes == null) {
			attlist = ge.getListables();
		} else {
			attlist = ge.getAttributeLines(attributes);
		}
		return groupSelection(entity, groupId, attlist, false, ge.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList groupSelection(MetaDataEntity groupEntity, int groupId, String attributes,
			boolean deleted, String attributeTest, Object value) {
		MetaDataEntity entity = getEntity(groupEntity.getGroupType());
		if (entity == null) {
			return new BeanMapList();
		}
		return groupSelection(groupEntity, groupId, getAttributesList(entity, attributes), false,
				entity.getAttributeLine(attributeTest), value);
	}

	public final BeanMapList groupSelection(String groupType, int groupId, String attributes, boolean deleted,
			String criteria, boolean distinct, String orders, IConnectionUserBean currentUser, int page, int limit) {
		MetaDataEntity entity = getEntity(groupType);
		if (entity == null) {
			return new BeanMapList();
		}
		MetaDataEntity ge = getEntity(entity.getGroupType());
		if (ge == null) {
			return new BeanMapList();
		}
		return groupSelection(entity, groupId, getAttributesList(ge, attributes), deleted, getCriteria(criteria),
				distinct, ge.getAttributeLines(orders), currentUser, page, limit);
	}

	public final boolean update(List<MetaDataAttribute> attributes, List<Object> values, ISearchCriteria criteria) {
		if ((attributes == null) || (attributes.size() == 0) || (values == null)
				|| (values.size() != attributes.size()) || (criteria == null)) {
			return false;
		}
		ICriteriaContext context = getContext((MetaDataEntity) attributes.get(0).getParent(), null);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return false;
		}
		return doUpdate(attributes, values, criteria, context);
	}

	public final boolean update(MetaDataEntity entity, List<MetaDataAttribute> attributes, List<Object> values,
			ISearchCriteria criteria) {
		ICriteriaContext context = getContext(entity, null);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return false;
			}
		}
		return doUpdate(attributes, values, criteria, context);
	}

	protected abstract boolean doUpdate(List<MetaDataAttribute> attributes, List<Object> values,
			ISearchCriteria criteria, ICriteriaContext context);

	public final BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			boolean distinct, List<ReferenceLine> orders, IConnectionUserBean currentUser, int page, int limit) {
		if ((page < 0) || (limit == 0) || (attributes == null) || (attributes.size() == 0)) {
			return new BeanMapList();
		}
		ICriteriaContext context = getContext(attributes.get(0).getOriginEntity(), currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return new BeanMapList();
			}
		}
		return doSelection(attributes, deleted, criteria, distinct, orders, page, limit, context);
	}

	public final BeanMapList selection(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria,
			boolean distinct, IConnectionUserBean currentUser, int page, int limit) {
		if ((page < 0) || (limit == 0) || (entity == null)) {
			return new BeanMapList();
		}
		ICriteriaContext context = getContext(entity, currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return new BeanMapList();
			}
		}
		return doSelection(new ArrayList<ReferenceLine>(), deleted, criteria, distinct, new ArrayList<ReferenceLine>(),
				page, limit, context);
	}

	/**
	 * Process to a paged selection.
	 * 
	 * @param attributes
	 *            can be empty. then no attributes must be returned.
	 * @param deleted
	 * @param criteria
	 * @param distinct
	 * @param orders
	 * @param page
	 * @param limit
	 * @param context
	 * @return
	 */
	protected abstract BeanMapList doSelection(List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, int page, int limit,
			ICriteriaContext context);

	public final int count(MetaDataEntity entity, boolean deleted, ISearchCriteria criteria, boolean distinct,
			IConnectionUserBean currentUser) {
		ICriteriaContext context = getContext(entity, currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return 0;
			}
		}
		return doCount(deleted, criteria, distinct, context);
	}

	/**
	 * Process to a "count" operation. 
	 * 
	 * @param deleted
	 * @param criteria
	 * @param distinct
	 * @param context
	 * @return
	 */
	protected abstract int doCount(boolean deleted, ISearchCriteria criteria, boolean distinct, ICriteriaContext context);

	public final BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders,
			IConnectionUserBean currentUser, int page, int limit) {
		if ((page < 0) || (limit == 0)) {
			return new BeanMapList();
		}
		MetaDataEntity entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			if (attributes == null) {
				attributes = entity.getListables();
			}
			ICriteriaContext context = getContext(entity, currentUser);
			if (criteria == null) {
				criteria = ConstantCriteria.TRUE;
			} else {
				criteria = criteria.reduce(context);
				if (ConstantCriteria.FALSE.equals(criteria)) {
					return new BeanMapList();
				}
			}
			return doLinkSelection(link, sourceId, attributes, deleted, criteria, distinct, orders, page, limit, context);
		}
		MetaDataAttribute att = entity.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return new BeanMapList();
		}
		// Send the "reversed selection" to the correct mapper !
		return entity.getMapper().selection(entity, attributes, deleted, new AndCriteria(new EqualCriteria(att.getCode(), sourceId), criteria),
				distinct, orders, currentUser, page, limit);
	}

	protected abstract BeanMapList doLinkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, int page,
			int limit, ICriteriaContext context);

	public final int linkCount(MetaDataLink link, int sourceId, boolean deleted, ISearchCriteria criteria,
			boolean distinct, IConnectionUserBean currentUser) {
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			ICriteriaContext context = getContext(link.getRefEntity(), currentUser);
			if (criteria == null) {
				criteria = ConstantCriteria.TRUE;
			} else {
				criteria = criteria.reduce(context);
				if (ConstantCriteria.FALSE.equals(criteria)) {
					return 0;
				}
			}
			return doLinkCount(link, sourceId, deleted, criteria, distinct, context);
		}
		MetaDataEntity e = link.getRefEntity();
		if (e == null) {
			return 0;
		}
		MetaDataAttribute att = e.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return 0;
		}
		return e.getMapper().count(e, deleted, new AndCriteria(new EqualCriteria(att.getCode(), sourceId), criteria),
				distinct, currentUser);
	}

	protected abstract int doLinkCount(MetaDataLink link, int sourceId, boolean deleted, ISearchCriteria criteria,
			boolean distinct, ICriteriaContext context);

	public final boolean test(MetaDataEntity entity, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		if (criteria == null) {
			return true;
		}
		ICriteriaContext context = getContext(entity, currentUser);
		criteria = criteria.reduce(context);
		if (ConstantCriteria.FALSE.equals(criteria)) {
			return false;
		}
		if (ConstantCriteria.TRUE.equals(criteria)) {
			return true;
		}
		return doCount(false, criteria, false, context) > 0;
	}

	@Override
	public boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, boolean deleted, IConnectionUserBean currentUser) {
		ICriteriaContext context = getContext(entity, currentUser);
		if (criteria == null) {
			criteria = new IdEqualCriteria(itemId);
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return false;
			}
			if (ConstantCriteria.TRUE.equals(criteria)) {
				criteria = new IdEqualCriteria(itemId);
			} else if (criteria instanceof AndCriteria) {
				((AndCriteria) criteria).add(new IdEqualCriteria(itemId));
			} else {
				criteria = new AndCriteria(criteria, new IdEqualCriteria(itemId));
			}
		}
		return doCount(deleted, criteria, false, context) > 0;
	}

	public final boolean test(MetaDataEntity entity, int itemId, ISearchCriteria criteria, IConnectionUserBean currentUser) {
		return test(entity, itemId, criteria, false, currentUser);
	}

	public final BeanMapList selection(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest,
			Object value) {
		if (attributeTest == null) {
			return new BeanMapList();
		}
		ICriteriaContext context = getContext(attributeTest.getOriginEntity(), null);
		return doSelection(attributes, deleted, getTestCriteria(attributeTest, value, context), false, null, 0, -1, context);
	}

	public final int count(MetaDataEntity entity, boolean deleted, ReferenceLine attributeTest, Object value) {
		ICriteriaContext context = getContext(entity, null);
		return doCount(deleted, getTestCriteria(attributeTest, value, context), false, context);
	}

	public final BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ReferenceLine attributeTest, Object value) {
		if (attributeTest == null) {
			return null;
		}
		ICriteriaContext context = getContext(attributeTest.getOriginEntity(), null);
		return doSelectionFirst(attributes, deleted, getTestCriteria(attributeTest, value, context), context);
	}

	public final BeanMap selectionFirst(String type, String attributes, boolean deleted, String criteria,
			IConnectionUserBean currentUser) {
		MetaDataEntity entity = getEntity(type);
		if (entity == null) {
			return null;
		}
		ICriteriaContext context = getContext(entity, currentUser);
		ISearchCriteria ctr = getCriteria(criteria).reduce(context);
		if (ConstantCriteria.FALSE.equals(ctr)) {
			return null;
		}
		return doSelectionFirst(getAttributesList(entity, attributes), deleted, ctr, context);
	}

	public final BeanMap selectionFirst(List<ReferenceLine> attributes, boolean deleted, ISearchCriteria criteria,
			IConnectionUserBean currentUser) {
		if ((attributes == null) || (attributes.size() == 0)) {
			return null;
		}
		ICriteriaContext context = getContext(attributes.get(0).getOriginEntity(), currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return null;
			}
		}
		return doSelectionFirst(attributes, deleted, criteria, context);
	}

	public final BeanMap selectionFirst(MetaDataEntity entity, List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, IConnectionUserBean currentUser) {
		ICriteriaContext context = getContext(entity, currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return null;
			}
		}
		return doSelectionFirst(attributes, deleted, criteria, context);
	}

	protected abstract BeanMap doSelectionFirst(List<ReferenceLine> attributes, boolean deleted,
			ISearchCriteria criteria, ICriteriaContext context);

	public final BeanMapList linkSelection(MetaDataLink link, int sourceId, List<ReferenceLine> attributes,
			boolean deleted, ReferenceLine attributeTest, Object value) {
		if (attributeTest == null) {
			return new BeanMapList();
		}
		MetaDataEntity entity = link.getRefEntity();
		if (entity == null) {
			return new BeanMapList();
		}
		String code = link.getMetadata().getString(MetaDataEntity.METADATA_REVERSELINK);
		if (code == null) {
			if ((attributes == null) || (attributes.size() == 0)) {
				attributes = entity.getAllAttributes();
				if (attributes.size() == 0) {
					return new BeanMapList();
				}
			}
			ICriteriaContext context = getContext(entity, null);
			return doLinkSelection(link, sourceId, attributes, deleted, getTestCriteria(attributeTest, value, context),
					false, null, 0, -1, context);
		}
		MetaDataAttribute att = entity.getAttribute(code);
		if ((att == null) || !link.getParent().equals(att.getRefEntity())) {
			return new BeanMapList();
		}
		return entity.getMapper().selection(attributes, deleted, new AndCriteria(new EqualCriteria(att.getCode(), sourceId), 
				new EqualCriteria(attributeTest.getCode(), value.toString())),
				false, null, null, 0, -1);
	}

	// Default Implementations...

	public boolean undelete(MetaDataEntity entity, int itemId) {
		return false;
	}

	public boolean groupAdd(MetaDataEntity groupEntity, int groupId, int itemId) {
		return false;
	}

	public boolean groupTest(MetaDataEntity groupEntity, int groupId, int itemId) {
		return false;
	}

	public boolean groupRemove(MetaDataEntity groupEntity, int groupId, int itemId) {
		return false;
	}

	public BeanMapList groupSelection(MetaDataEntity groupEntity, int groupId, List<ReferenceLine> attributes,
			boolean deleted, ReferenceLine attributeTest, Object value) {
		return new BeanMapList();
	}

	public final BeanMapList groupSelection(MetaDataEntity groupEntity, int groupId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders,
			IConnectionUserBean currentUser, int page, int limit) {
		if ((page < 0) || (limit == 0) || (attributes == null) || (attributes.size() == 0)) {
			return new BeanMapList();
		}
		ICriteriaContext context = getContext(groupEntity, currentUser);
		if (criteria == null) {
			criteria = ConstantCriteria.TRUE;
		} else {
			criteria = criteria.reduce(context);
			if (ConstantCriteria.FALSE.equals(criteria)) {
				return new BeanMapList();
			}
		}
		return doGroupSelection(groupEntity, groupId, attributes, deleted, criteria, distinct, orders, page, limit,
				context);
	}

	protected BeanMapList doGroupSelection(MetaDataEntity groupEntity, int groupId, List<ReferenceLine> attributes,
			boolean deleted, ISearchCriteria criteria, boolean distinct, List<ReferenceLine> orders, int page,
			int limit, ICriteriaContext context) {
		return new BeanMapList();
	}

}
