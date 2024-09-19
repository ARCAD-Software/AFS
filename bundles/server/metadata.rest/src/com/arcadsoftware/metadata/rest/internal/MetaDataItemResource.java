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
package com.arcadsoftware.metadata.rest.internal;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.restlet.data.Form;
import org.restlet.data.Language;
import org.restlet.data.Method;
import org.restlet.data.Reference;
import org.restlet.data.Status;
import org.restlet.representation.Representation;
import org.restlet.representation.Variant;
import org.restlet.resource.ResourceException;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.metadata.IByPassListener;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataLinkingListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.IMetaDataSelectionListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;
import com.arcadsoftware.metadata.MetaDataTest;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.criteria.AndCriteria;
import com.arcadsoftware.metadata.criteria.ConstantCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.metadata.criteria.IdEqualCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.metadata.criteria.OrCriteria;
import com.arcadsoftware.metadata.rest.DataItemResource;
import com.arcadsoftware.metadata.rest.DataParentResource;

/*
 * Gestion des données :
 * 
 *  /data/{type}/{id} avec support multi-sélection.
 *  - get : lecture d'une ou plusieurs données. Supporte tous les paramètres de sélection (dont le critère et les jointures sur attributs).
 *  - put ou post : modification avec désuppression.
 *  - delete : suppression avec support de la suppression "hard" par paramètres. Le support réel de cette option dépends du mapper.
 *  
 *  /data/{type}/{id}/{linkcode}
 *  - get : liste les éléments associés. Supporte tous les paramètres de sélection (dont le critère et les jointures sur attributs).
 *  - post ou put : ajout d'une association.
 *  
 *  /data/{type}/{id}/{linkcode}/{iid}
 *  - get : teste l'existance d'une association.
 *  - put ou post : ajout d'une association.
 *  - delete : suppression d'une association.
 *  
 *  /data/{type}/{id}/{code}/{code}... accès direct sur référence
 *  - get : lecture d'une ou plusieurs entités. Supporte tous les paramètres de sélection (dont le critère et les jointures sur attributs).
 *  - put ou post : modification avec désuppression.
 *  - delete : suppression avec support de la suppression "hard" par paramètres. Le support réel de cette option dépends du mapper.
 *  
 */
public class MetaDataItemResource extends DataItemResource {
	
	private static final String PARAM_HARDDELETE = "harddelete"; //$NON-NLS-1$
	private static final String PARAM_DELETETARGETS = "deletetargets"; //$NON-NLS-1$

	private MetaDataLink link;
	private MetaDataEntity linkEntity;
	private List<BeanMap> linkeds;
	private String codes;
	private ReferenceLine attLine;
	
	@Override
	protected void doInit() throws ResourceException {
		// prétraitement de la fin de l'url.
		preprocessCodes();
		super.doInit();
		if (!isExisting()) {
			return;
		}
		// Gestion linkcode et iid !
		if (codes != null) {
			doInitLink();
			if (link == null) {
				// Traitement des références.
				doInitAttributesLine();
			} else if ((linkeds != null) && (linkeds.size() == 0)) {
				setExisting(false);
			}
		}
		if (isExisting() && Method.HEAD.equals(getMethod()) && (linkeds == null) && //
				(((link == null) && hasUpdateDate(getEntity())) || //
						((link != null) && hasUpdateDate(linkEntity)))) {
			computeLastModificationDate();
		}
	}

	private boolean hasUpdateDate(MetaDataEntity entity) {
		return (entity != null) && (entity.getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) || //
				(entity.getMetadata().get("updateCol") != null)); //$NON-NLS-1$;
	}

	private void computeLastModificationDate() {
		Form form = getRequestForm();
		MetaDataEntity entity = getEntity();
		final BeanMapList result;
		if (link != null) {
			int page = getFirst(form);
			int limit = getPageCount(form, page);
			// check other parameters... 
			boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
			boolean distinct = isParameter(form, "distincts"); //$NON-NLS-1$
			ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
			if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
				criteria = link.getRightList(true);
			} else {
				ISearchCriteria rc = link.getRightList(true);
				if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
					criteria = new AndCriteria(criteria, rc);
				}
			}
			result = new BeanMapPartialList();
			for (BeanMap item: getItems()) {
				int count = getEntity().getMapper().linkCount(link, item.getId(), deleted, criteria, distinct, getUser());
				// select when needed. (fusion of results).
				if ((page < count) && (limit != 0)) {
					// Select...
					BeanMapList list = getEntity().getMapper().linkSelection(link, item.getId(), new ArrayList<ReferenceLine>(), deleted, criteria, distinct, null, getUser(), page, limit);
					if (list != null) {
						result.addAll(list);
					}
					// if we get less result than expected we decrement the number of desired results.
					if ((limit > 0) && (result.size() < limit)) {
						limit = limit - result.size();
						page = 0;
					} else {
						limit = 0;
					}
				}
				// Decrement the first result needed.
				if (page > 0) {
					page = page - count;
					if (page < 0) { // Blindage
						page = 0;
					}
				}
			}
			entity = linkEntity;
		} else {
			ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
			if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
				criteria = entity.getRightRead();
			} else {
				ISearchCriteria rc = entity.getRightRead();
				if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
					criteria = new AndCriteria(criteria, rc);
				}
			}
			OrCriteria or = new OrCriteria();
			for (BeanMap item: getItems()) {
				or.add(new IdEqualCriteria(item.getId()));
			}
			if (criteria instanceof AndCriteria) {
				((AndCriteria) criteria).add(or);
			} else {
				criteria = new AndCriteria(criteria, or);
			}
			// Get the result offset.
			int first = getFirst(form);
			int number = getPageCount(form, first);
			// check other parameters... 
			boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
			boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
			// Execution de la sélection proprement dite.
			result = entity.dataSelection(new ArrayList<ReferenceLine>(), deleted, criteria, distincts, null, getUser(), first, number);
		}
		Date date = new Date(0l);
		// Return EPOCH for empty results...
		if (result.size() > 0) {
			for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(entity.getType())) {
				listener.onSelection(entity, result, getUser(), Language.ENGLISH_US);
			}
			for (BeanMap b: result) {
				Date d = b.getDate();
				if ((d != null) && date.before(d)) {
					date = d;
				}
			}
		}
		setLastModification(date);
	}

	private void doInitAttributesLine() {
		if (attLine == null) {
			setExisting(false);
		} else if (attLine.getLast() instanceof MetaDataAttribute) {
			MetaDataEntity e = attLine.getLastAttribute().getRefEntity();
			if (e == null) {
				setExisting(false);
			} else {
				switchEntity(e);
				if (getItems().size() == 0) {
					setExisting(false);
				}
			}
		} else {
			setExisting(false);
		}
	}

	private void doInitLink() {
		String lids = null;
		String code = null;
		if (codes.indexOf('/') > 0) {
			String[] s = codes.split("/"); //$NON-NLS-1$
			if (s.length > 0) {
				code = s[0];
				if (s.length > 1) {
					lids = s[1];
					for (int i = 2;i < s.length; i++) {
						lids = lids + '+' + s[i];
					}
				}
			}
		} else {
			code = codes;
		}
		//code = Reference.decode(code);
		// Traitement des associations.
		link = getEntity().getLink(code);
		if (link != null) {
			linkEntity = link.getRefEntity();
			MetaDataAttribute att = getCodeAttribute(linkEntity);
			ReferenceLine refline = null;
			if (att != null) {
				refline = new ReferenceLine(att);
			}
			if ((lids != null) && (lids.length() > 0)) {
				linkeds = new ArrayList<BeanMap>();
				for (String lid: lids.split("\\+")) { //$NON-NLS-1$
					if (lid.length() > 0) {
						BeanMap bean = null;
						try {
							bean = linkEntity.getMapper().selection(linkEntity, Integer.parseInt(lid), (List<ReferenceLine>) null, true);
						} catch (NumberFormatException e) {}
						if ((bean == null) && (refline != null)) {
							bean = linkEntity.getMapper().selectionFirst(linkEntity, null, true, refline, Reference.decode(lid));
						}
						if (bean != null) {
							linkeds.add(bean);
						}
					}
				}
			} else {
				linkeds = getLinksFromForm(getRequestForm());
			}
		}
	}

	private void preprocessCodes() {
		codes = getReference().getRemainingPart(true, false);
		if (codes != null) {
			if (codes.length() == 0) {
				codes = null;
			} else {
				while (codes.charAt(0) == '/') {
					codes = codes.substring(1);
					if (codes.length() == 0) {
						codes = null;
						break;
					}
				}
			}
			if (codes != null) {
				while (codes.charAt(codes.length() - 1) == '/') {
					codes = codes.substring(0, codes.length() - 1);
					if (codes.length() == 0) {
						codes = null;
						break;
					}
				}
			}
		}
	}

	private void switchEntity(MetaDataEntity e) {
		BeanMapList list = new BeanMapList();
		String code = attLine.getCode();
		for (BeanMap bean: getItems()) {
			int id = bean.getInt(code);
			if (id > 0) {
				list.addUnique(e.getMapper().selection(e, id, (String)null, false));
			}
		}
		setItems(list);
		setEntity(e);
	}

	@Override
	protected List<ReferenceLine> getPreloadedAttributes() {
		if (codes != null) {
			// Traitement des références.
			attLine = getEntity().getReferenceLine(Reference.decode(codes).replace('/', '.'));
			if (attLine != null) {
				if ((attLine.size() == 0) || (!attLine.isAttributeList()) || attLine.isFinal() || attLine.isHidden()) {
					attLine = null;
					return null;
				}
				ArrayList<ReferenceLine> list = new ArrayList<ReferenceLine>(1);
				list.add(attLine);
				return list;
			}
		}
		return null;
	}

	@Override
	protected Representation delete(Variant variant) throws ResourceException {
		if (!isExisting()) {
			setStatus(Status.SUCCESS_NO_CONTENT);
			return null;
		}
		Language language = getClientPreferedLanguage();
		Form form = getRequestForm();
		if (linkeds != null) {
			// Suppression de certains liens.
			return deleteLinks(variant, form, language);
		}
		if (link != null) {
			// Suppression de tous les liens.
			return deleteAllLinks(variant, form, language);
		}
		MetaDataEntity entity = getEntity();
		if (entity.isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		final boolean hardelete = isParameter(form, PARAM_HARDDELETE); //$NON-NLS-1$
		boolean noerrors = true;
		List<IMetaDataDeleteListener> listeners = Activator.getInstance().getDeleteListener(entity.getType());
		for (BeanMap item: getItems()) {
			if (hardelete || !item.isDeleted()) {
				if (!hasRightDelete(entity, item, language)) {
					if (noerrors) {
						setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nodelete", language)); //$NON-NLS-1$
						noerrors = false;
					}
					continue;
				}
				boolean cnt = false;
				boolean bypass = false;
				for(IMetaDataDeleteListener listener: listeners) {
					if (!listener.testDeletion(entity, item, getUser(), language)) {
						cnt = true;
						break;
					}
					if (listener instanceof IByPassListener) {
						bypass = true;
					}
				}
				if (cnt) {
					continue;
				}
				broadcastUserAction("logDelete", item); //$NON-NLS-1$
				if (!bypass) {
					entity.getMapper().delete(item, hardelete);
				}
				item.setDeleted(true);
				Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERDELETE, entity, item, getUser(), language);
				for(IMetaDataDeleteListener listener: listeners) {
					listener.postDeletion(entity, item, getUser(), language);
				}
				Activator.getInstance().fireDeleteEvent(entity, item, getUser(), hardelete || 
						(entity.getMetadata().get("deleteCol") == null));
			}
		}
		if (noerrors) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		}
		return null;
	}

	/**
	 * Delete All Links. Optionnaly delete also linked items.
	 * @param variant
	 * @param form
	 * @param language
	 * @return
	 * @throws ResourceException
	 */
	private Representation deleteAllLinks(Variant variant, Form form, Language language) throws ResourceException {
		List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean deleted = false;
		boolean hardelete = isParameter(form, PARAM_HARDDELETE); //$NON-NLS-1$
		boolean deleteitems = isParameter(form, PARAM_DELETETARGETS); //$NON-NLS-1$
		for (BeanMap item: getItems()) {
			// select linked items
			BeanMapList linkedItems = getEntity().getMapper().linkSelection(link, item.getId());
			if (linkedItems != null) {
				for (BeanMap linkedItem: linkedItems) {
					if (!linkEntity.getMapper().test(linkedItem, link.getRightCreate(true), getUser())) {
						// TODO les tests de droits sur les associations devrait pouvoir être appliqué aussi à la source !!!
						errors = true;
					} else {
						boolean doit = true;
						boolean byPass = false;
						for (IMetaDataLinkingListener listener: listeners) {
							if (!listener.testUnlink(link, item, linkedItem, getUser(), language)) {
								doit = false;
								errors = true;
								break;
							}
							if (listener instanceof IByPassListener) {
								byPass = true;
							}
						}
						if (doit) {
							broadcastUserAction(link, "logUnlink", item, linkedItem); //$NON-NLS-1$
							boolean fireEvent = byPass;
							if (deleteitems) {
								broadcastUserAction("logDelete", item); //$NON-NLS-1$
								if (!byPass) {
									getEntity().getMapper().delete(linkedItem, hardelete);
									// check if cascade deletion also removed the link.
									if (!getEntity().getMapper().linkTest(link, item.getId(), linkedItem.getId())) {
										fireEvent = true;
									}
								}
							}
							if ((!byPass) && getEntity().getMapper().linkRemove(link, item.getId(), linkedItem.getId())) {
								fireEvent = true;
							}
							if (fireEvent) {
								deleted = true;
								Activator.getInstance().fireUnlinkEvent(getEntity(), link, item, linkedItem, getUser());
							}
						}
					}
				}
			}
		}
		if (errors) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nounlink", language)); //$NON-NLS-1$
		} else if (deleted) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
		return null;
	}

	@Override
	protected Representation get(Variant variant) throws ResourceException {
		if (isXSD(variant)) {
			return MetaDataParentResource.getXSDFileRepresentation(getEntity(), getClientPreferedLanguage());
		}
		Language language = getClientPreferedLanguage();
		Form form = getRequestForm();
		if (linkeds != null) {
			return testLinks(variant, form, language);
		}
		if (link != null) {
			return listLinks(variant,form,language);
		}
		MetaDataEntity entity = getEntity();
		// Build the requested attribute list.
		List<ReferenceLine> attributes = getSelectedAttributes(getEntity(), false);
		// Build the requested sort order list.
		List<ReferenceLine> orders;
		if (attributes.size() == 0) {
			orders = new ArrayList<ReferenceLine>();
		} else {
			orders = entity.getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
		}
		Iterator<ReferenceLine> itt = orders.iterator();
		while (itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				if (a.getRightRead(false) != null) {
					// TODO Ce test devrait être complété par un test sur la jointure !
					MetaDataEntity e = (MetaDataEntity) a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
						continue;
					}
				}
				// Si la colonne n'est pas sélectionnée on l'ajoute.
				if (!attributes.contains(itt.next())) {
					attributes.add(att);
				}
			}
		}
		// Build the search criteria
		boolean nocriteria = false;
		ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
		if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
			nocriteria = true;
			criteria = entity.getRightRead();
		} else {
			ISearchCriteria rc = entity.getRightRead();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		OrCriteria or = new OrCriteria();
		for (BeanMap item: getItems()) {
			or.add(new IdEqualCriteria(item.getId()));
		}
		if (criteria instanceof AndCriteria) {
			((AndCriteria) criteria).add(or);
		} else {
			criteria = new AndCriteria(criteria, or);
		}
		// Get the result offset.
		int first = getFirst(form);
		int number = getPageCount(form, first);
		// check other parameters... 
		boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
		boolean translate = !isParameter(form, "notranslation"); //$NON-NLS-1$
		// Ajouter les ".code" nécessaire aux "translate"...
		if (translate) {
			for(ReferenceLine rl: attributes) {
				if (rl.isTranslatable()) {
					ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !attributes.contains(trl)) {
						attributes.add(trl);
					}
				}
			}
		}
		// Execution de la sélection proprement dite.
		BeanMapList result = entity.dataSelection(attributes, deleted, criteria, distincts, orders, getUser(), first, number);
		if (translate) {
			for(ReferenceLine rl: attributes) {
				if (rl.isTranslatable()) {
					for (BeanMap bm: result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		// Le test "read" n'a ici pas d'influence directed sur la suite du traitement (il peut seulement modifier le résultat):
		Activator.getInstance().test(MetaDataTest.EVENTCODE_READ, entity, result, getUser(), language);
		if ((entity.getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) && (result.size() > 1)) {
			Activator.getInstance().fireSelectionEvent(getEntity(), result, getUser());
		}
		if (result.size() > 0) {
			for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(getEntity().getType())) {
				listener.onSelection(getEntity(), result, getUser(), language);
			}
		}
		// mise en forme du résultat.
		switch (result.size()) {
		case 0:
			// Générer un message d'erreur uniquement si l'on est SUR que le résultat est vide 
			// parce que le test du droit d'accès à échoué !
			// (la liste pourrait être vide parce que la table est vide...) 
			if (nocriteria && (getItems().size() >= 1)) {
				if (entity.dataHasRightRead(getItems().get(0).getId(), getUser())) { 
					setStatus(Status.CLIENT_ERROR_NOT_FOUND, Activator.getMessage("info.deleted", language)); //$NON-NLS-1$
				} else {
					setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.noread", language)); //$NON-NLS-1$
				}
				return null;
			}
			// Empty list...
			return getRepresentation(variant, form, result, language, false);
		case 1:
			return getRepresentation(variant, form, result.get(0), language, false);
		default: 
			return getRepresentation(variant, form, result, language, false);
		}
	}

	protected int getPageCount(Form form, int first) {
		String s = form.getFirstValue("pagecount"); //$NON-NLS-1$
		if (s == null) {
			s = getAttribute("pagecount"); //$NON-NLS-1$
		}
		if (s == null) {
			s = form.getFirstValue("limit"); //$NON-NLS-1$
			if (s == null) {
				s = getAttribute("limit"); //$NON-NLS-1$
			}
			if (s != null) {
				try {
					return Integer.parseInt(s) - first;
				} catch (NumberFormatException e) {}
			}
		} else {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return getDefaultPageCount(first);
	}

	protected int getDefaultPageCount(int first) {
		return 20;
	}

	protected int getFirst(Form form) {
		String s = form.getFirstValue("pagestart"); //$NON-NLS-1$
		if (s == null) {
			s = getAttribute("pagestart"); //$NON-NLS-1$
		}
		if (s == null) {
			s = form.getFirstValue("offset"); //$NON-NLS-1$
		}
		if (s == null) {
			s = getAttribute("offset"); //$NON-NLS-1$
		}
		if (s != null) {
			try {
				return Integer.parseInt(s);
			} catch (NumberFormatException e) {}
		}
		return 0;
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (!isExisting()) {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
			return null;
		}
		Language language = getClientPreferedLanguage();		
		Form form = getRequestForm();
		if (link != null) {
			return addLinks(variant, form, language);
		}
		MetaDataEntity entity = getEntity();
		if (entity.isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		// Set a default status.
		setStatus(Status.SUCCESS_OK);
		ArrayList<MetaDataAttribute> attlist = new ArrayList<MetaDataAttribute>();
		BeanMap values = entity.formToBean(form, attlist);
		List<IMetaDataModifyListener> listeners = Activator.getInstance().getModifyListener(getType());
		BeanMapList list = new BeanMapList(getItems().size());
		final boolean undelete = isParameter(form, "undelete"); //$NON-NLS-1$
		if (undelete) {
			// Undelete is not an attribute...
			values.remove("undelete"); //$NON-NLS-1$
		}
		for (BeanMap item: getItems()) {
			BeanMap result = put(variant, form, entity, item, values, attlist, language, listeners, undelete);
			if (result != null) {
				broadcastUserAction("logUpdate", result); //$NON-NLS-1$
				list.add(result);
			}
		}
		if (Status.SUCCESS_OK.equals(getStatus())) {
			// If there is some errors we do not have to generate a result even if some of the modification can be completed.
			switch (list.size()) {
				case 0:
					setStatus(Status.SUCCESS_NO_CONTENT);
					return null;
				case 1:
					return getRepresentation(variant, form, list.get(0), language, false);
				default:
					return getRepresentation(variant, form, list, language, false);
			}
		} else {
			return null;
		}
	}

	private BeanMap put(Variant variant, Form form, MetaDataEntity entity, BeanMap item, BeanMap values,
			ArrayList<MetaDataAttribute> attlist, Language language, List<IMetaDataModifyListener> listeners, boolean undelete) {
		if (!hasRightUpdate(entity, item, undelete, true, language)) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.noupdate", language)); //$NON-NLS-1$
			return null;
		}
		// Support undelete !
		if (undelete) {
			if (!item.isDeleted()) {
				// Just log a message, this operation is idempotent !
				Activator.getInstance().warn("Undeleting an already not deleted data: " + item.getType() + '/' + item.getId());
			}
			// Check that the undeleted values go against an "unique value" constraint...
			// FIXME We should call the Entity Groovy test before this test because it may alter the values !!!
			for (MetaDataAttribute attribute: entity.getAttributes().values()) {
				if (attribute.getMetadata().getBoolean(MetaDataEntity.METADATA_UNIQUE)) {
					Object value = values.get(attribute.getCode());
					if (value == null) {
						value = item.get(attribute.getCode());
					}
					if ((value != null) && (value.toString().length() > 0) &&  // ignore null values...
							(getEntity().dataCount(false, attribute.getCode(), value) > 0)) {
						throw new ResourceException(Status.CLIENT_ERROR_PRECONDITION_FAILED, //
								String.format(Activator.getMessage("error.uniqueattribute.undelete", language), //$NON-NLS-1$ 
										attribute.getName(language), value, attribute.getCode()));
					}
				}
			}
			entity.getMapper().undelete(item);
			item.setDeleted(false);
			Activator.getInstance().fireUndeleteEvent(getEntity(), item, getUser());
		}
		// attlist must not be changed (it is used by others updates...)
		ArrayList<MetaDataAttribute> attlistex = new ArrayList<MetaDataAttribute>(attlist.size());
		for (MetaDataAttribute att: attlist) {
			if ((att.getRightUpdate(false) == null) || //
					entity.getMapper().test(entity, item.getId(), att.getRightUpdate(false), getUser())) {
				attlistex.add(att);
			} else {
				item.remove(att.getCode());
			}
		}
		switch (doUpdateTest(listeners, item, values, attlistex, language)) {
		case 0:
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.missingattributes", language)); //$NON-NLS-1$
			return null;
		case -1:
			values.forceId(item.getId());
			values.setDeleted(item.isDeleted());
			break;
		case 1:
			// Check "unique" values constraints
			if (!undelete) {
				// Already done if undelete is true (see above !)
				ISearchCriteria invariantCriteria = new NotCriteria(new IdEqualCriteria(item.getId()));
				for (MetaDataAttribute attribute: attlist) {
					if (attribute.getMetadata().getBoolean(MetaDataEntity.METADATA_UNIQUE)) {
						Object value = values.get(attribute.getCode());
						if ((value != null) && (value.toString().length() > 0)) { // ignore null values...
							// Removes the current element from the selection (avoids updates on the same value)
							EqualCriteria codeEqual = new EqualCriteria();
							codeEqual.setAttribute(attribute.getCode());
							if (MetaDataAttribute.TYPE_INTEGER.equals(attribute.getType()) || MetaDataAttribute.TYPE_INT.equals(attribute.getType())) {
								try {
									codeEqual.setIntval(new Integer(value.toString()));
								} catch (NumberFormatException e) {
									codeEqual.setValue(value.toString());
								}
							} else {
								codeEqual.setValue(value.toString());
							}
							if (getEntity().dataCount(false, new AndCriteria(invariantCriteria , codeEqual), false, getUser()) > 0) {
								throw new ResourceException(Status.CLIENT_ERROR_PRECONDITION_FAILED, //
										String.format(Activator.getMessage("error.uniqueattribute.update", language), //$NON-NLS-1$ 
												attribute.getName(language), value, attribute.getCode()));
							}
						}
					}
				}
			}
			values.forceId(item.getId());
			values.setDeleted(item.isDeleted());
			getMapper().update(values);
		}
		if (values.getId() == 0) {
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.badattributes", language)); //$NON-NLS-1$
			return null;
		}
		doPostUpdateTreatment(listeners, item, values, attlistex, language);
		return values;
	}

	private int doUpdateTest(List<IMetaDataModifyListener> listeners, BeanMap oldValue, BeanMap result, ArrayList<MetaDataAttribute> list, Language language) {
		MetaDataEntity entity = getEntity();
		boolean bypass = false;
		for (IMetaDataModifyListener listener: listeners) {
			if (!listener.testModification(entity, oldValue, result, list, getUser(), language)) {
				return 0;
			}
			if (listener instanceof IByPassListener) {
				bypass = true;
			}
		}
		if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREUPDATE, entity, oldValue, result, list, getUser(), language)) {
			return 0;
		}
		if (bypass) {
			return -1;
		}
		return 1;
	}

	private void doPostUpdateTreatment(List<IMetaDataModifyListener> listeners, BeanMap oldValue, BeanMap result, ArrayList<MetaDataAttribute> list, Language language) {
		// TODO translate !?!??!! (en théorie les attributs translate ne sont pas modifiés !
		MetaDataEntity entity = getEntity();
		for (IMetaDataModifyListener listener:listeners) {
			listener.postModification(entity, oldValue, result, list, getUser(), language);
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERUPDATE, entity, oldValue, result, list, getUser(), language);
		Activator.getInstance().fireUpdateEvent(entity, oldValue, result, getUser());
		for (MetaDataAttribute att: entity.getAttributes().values()) {
			// Remove all hidden attributes before to return them to the client.
			if (!att.isPublic()) {
				result.remove(att.getCode());
			}
			// FIX: Convert Boolean values back to true boolean value...
			if (MetaDataAttribute.TYPE_BOOLEAN.equals(att.getType()) && result.contains(att.getCode())) {
				result.put(att.getCode(), result.getBoolean(att.getCode()));
			}
		}
	}

	private Representation deleteLinks(Variant variant, Form form, Language language) {
		List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean deleted = false;
		for (BeanMap item: getItems()) {
			for (BeanMap linked: linkeds) {
				if (!linkEntity.getMapper().test(linked, link.getRightCreate(true), getUser())) {
					// TODO les tests de droits sur les associations devrait pouvoir être appliqué aussi à la source !!!
					errors = true;
				} else {
					boolean doit = true;
					boolean bypass = false;
					for (IMetaDataLinkingListener listener: listeners) {
						if (!listener.testUnlink(link, item, linked, getUser(), language)) {
							doit = false;
							errors = true;
							break;
						}
						if (listener instanceof IByPassListener) {
							bypass = true;
						}
					}
					if (doit) {
						broadcastUserAction(link, "logUnlink", item, linked); //$NON-NLS-1$
						if (bypass || getEntity().getMapper().linkRemove(link, item.getId(), linked.getId())) {
							deleted = true;
							Activator.getInstance().fireUnlinkEvent(getEntity(), link, item, linked, getUser());
						}
					}
				}
			}
		}
		if (errors) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nounlink", language)); //$NON-NLS-1$
		} else if (deleted) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
		return null;
	}

	private Representation listLinks(Variant variant, Form form, Language language) {
		// Liste une association, prend en charge le multi-id et la pagination.
		List<ReferenceLine> attributes = getSelectedAttributes(linkEntity, true);
		// Get the result offset.
		int page = getFirst(form);
		int limit = getPageCount(form, page);
		// check other parameters... 
		boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		boolean distinct = isParameter(form, "distincts"); //$NON-NLS-1$
		boolean translate = !isParameter(form, "notranslation"); //$NON-NLS-1$
		List<ReferenceLine> orders = linkEntity.getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
		Iterator<ReferenceLine> itt = orders.iterator();
		while (itt.hasNext()) {
			ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				MetaDataAttribute a = att.getLastAttribute();
				if (a.getRightRead(false) != null) {
					// TODO Ce test devrait être complété par un test sur la jointure !
					MetaDataEntity e = (MetaDataEntity)a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
						continue;
					}
				}
				// Si la colonne n'est pas sélectionnée on l'ajoute.
				if (!attributes.contains(att)) {
					attributes.add(att);
				}
			}
		}
		ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
		if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
			criteria = link.getRightList(true);
		} else {
			ISearchCriteria rc = link.getRightList(true);
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		if (translate) {
			List<ReferenceLine> allattributes = new ArrayList<ReferenceLine>(attributes);
			for (ReferenceLine rl: attributes) {
				if (rl.isTranslatable()) {
					ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !allattributes.contains(trl)) {
						allattributes.add(trl);
					}
				}
			}
			attributes = allattributes;
		}
		BeanMapPartialList result = new BeanMapPartialList();
		for (BeanMap item: getItems()) {
			int count = getEntity().getMapper().linkCount(link, item.getId(), deleted, criteria, distinct, getUser());
			result.incrementTotal(count);
			// select when needed. (fusion of results).
			if ((page < count) && (limit != 0)) {
				// If we are starting to load the result list, we set the rank.
				if (result.size() == 0) {
					result.setRank(result.getTotal() - count + page);
				}
				// Select...
				BeanMapList list = getEntity().getMapper().linkSelection(link, item.getId(), attributes, deleted, criteria, distinct, orders, getUser(), page, limit);
				if (list != null) {
					result.addAll(list);
				}
				// if we get less result than expected we decrement the number of desired results.
				if ((limit > 0) && (result.size() < limit)) {
					limit = limit - result.size();
					page = 0;
				} else {
					limit = 0;
				}
			}
			// Decrement the first result needed.
			if (page > 0) {
				page = page - count;
				if (page < 0) { // Blindage
					page = 0;
				}
			}
		}
		if (result.size() == 0) {
			return getRepresentation(variant, form, result, language, true);
		}
		if (translate) {
			for(ReferenceLine rl:attributes) {
				if (rl.isTranslatable()) {
					for (BeanMap bm:result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_LIST, linkEntity, result, getUser(), language);
		if (getEntity().getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) {
			Activator.getInstance().fireSelectionEvent(linkEntity, result, getUser());
		}
		for (IMetaDataSelectionListener listener: Activator.getInstance().getSelectionListener(linkEntity.getType())) {
			listener.onSelection(linkEntity, result, getUser(), language);
		}
		return getRepresentation(variant, form, result, language, true);
	}

	private Representation testLinks(Variant variant, Form form, Language language) {
		boolean tested = false;
		for (BeanMap item: getItems()) {
			for (BeanMap linked: linkeds) {
				if (getEntity().getMapper().linkTest(link, item.getId(), linked.getId())) {
					tested = true;
				}
			}
		}
		if (tested) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
		return null;
	}

	private Representation addLinks(Variant variant, Form form, Language language) {
		if (linkeds == null) {
			linkeds = getLinksFromForm(form);
			if (linkeds == null) {
				setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.missinglinkrefs", language)); //$NON-NLS-1$
				return null;
			}
		}
		List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean added = false;
		for (BeanMap item: getItems()) {
			for (BeanMap linked: linkeds) {
				if (getEntity().getMapper().linkTest(link, item.getId(), linked.getId())) {
					// If the data are already linked we ignore this operation, but we return an "Ok" message.
					Activator.getInstance().debug("Metadata link already exist: " + link.getParent().getType() + //
							'.' + link.getCode() + " [" + item.getId() + " -> " + linked.getId() + ']'); //$NON-NLS-1$ //$NON-NLS-2$
					added = true;
				} else if (!linkEntity.getMapper().test(linked, link.getRightCreate(true), getUser())) {
					// TODO les tests de droits sur les associations devraient pouvoir être appliqué aussi à la source !!!
					errors = true;
				} else {
					boolean doit = true;
					for(IMetaDataLinkingListener listener: listeners) {
						if (!listener.testLink(link, item, linked, getUser(), language)) {
							doit = false;
							errors = true;
							break;
						}
					}
					// FIXME Si l'attribut d'un reverse-link est aussi déclaré comme unique alors il faut tester ça nouvelle valeur.
					if (doit && getEntity().getMapper().linkAdd(link, item.getId(), linked.getId())) {
						broadcastUserAction(link, "logLink", item, linked); //$NON-NLS-1$
						added = true;
						Activator.getInstance().fireLinkEvent(getEntity(), link, item, linked, getUser());
					}
				}
			}
		}
		if (errors) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.nolink", language)); //$NON-NLS-1$
		} else if (added) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		} else {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
		}
		return null;
	}

	private List<BeanMap> getLinksFromForm(Form form) {
		String[] ids = form.getValuesArray(KEY_ID, true);
		if ((ids == null) || (ids.length == 0)) {
			return null;
		}
		ArrayList<BeanMap> result = new ArrayList<BeanMap>(ids.length);
		for (String id: ids) {
			if (id.length() > 0) {
				BeanMap bean = null;
				try {
					bean = linkEntity.getMapper().selection(linkEntity, Integer.parseInt(id), (List<ReferenceLine>) null, true);
				} catch (NumberFormatException e) {}
				if (bean != null) {
					result.add(bean);
				}
			}
		}
		ids = form.getValuesArray(ATTRIBUTE_CODE, true);
		if ((ids != null) && (ids.length > 0)) {
			MetaDataAttribute att = linkEntity.getAttribute(ATTRIBUTE_CODE);
			ReferenceLine refline = null;
			if (att != null) {
				refline = new ReferenceLine(att);
			}
			for(String code: ids) { //$NON-NLS-1$
				if (code.length() > 0) {
					BeanMap bean = linkEntity.getMapper().selectionFirst(linkEntity, null, true, refline, code);
					if (bean != null) {
						result.add(bean);
					}
				}
			}
		}
		if (result.size() == 0) {
			return null;
		}
		return result;
	}
}
