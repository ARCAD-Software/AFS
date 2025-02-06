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
import org.restlet.engine.header.DateWriter;
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
import com.arcadsoftware.metadata.IMetaDataUndeleteListener;
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
 * Data management:
 * 
 * /data/{type}/{id} with multi-selection support.
 * - get: read one or more data. Supports all selection parameters (including criteria and joins on attributes).
 * - put or post: modify with undelete.
 * - delete: delete with support for "hard" delete by parameters. The actual support for this option depends on the mapper.
 *
 * /data/{type}/{id}/{linkcode}/{linkcode}...
 * - get: list associated elements. Supports all selection parameters (including criteria and joins on attributes).
 * - post or put: add an association.
 * 
 * /data/{type}/{id}/{linkcode}/{linkcode}.../{iid}
 * - get: test the existence of an association.
 * - put or post: add an association.
 * - delete: delete an association.
 * 
 * /data/{type}/{id}/{code}/{code}... direct access on reference
 * - get : read one or more entities. Supports all selection parameters (including criteria and joins on attributes).
 * - put or post : modification with undeletion.
 * - delete : deletion with support for "hard" deletion by parameters. The actual support for this option depends on the mapper.
 * 
 */
public class MetaDataItemResource extends DataItemResource {

	private static final String PARAM_HARDDELETE = "harddelete"; //$NON-NLS-1$
	private static final String PARAM_DELETETARGETS = "deletetargets"; //$NON-NLS-1$

	private List<MetaDataLink> links;
	private MetaDataEntity linkEntity;
	private List<BeanMap> linkeds;
	private String codes;
	private ReferenceLine attLine;

	@Override
	protected void doInit() throws ResourceException {
		// Pre-processing of the end of the url.
		codes = preprocessRemainingPart();
		super.doInit();
		if (!isExisting()) {
			return;
		}
		// Management of link-codes and link-ids !
		if (codes != null) {
			doInitLink();
			if (links == null) {
				// References management.
				doInitAttributesLine();
			} else if ((linkeds != null) && (linkeds.size() == 0)) {
				setExisting(false);
			}
		}
		if (isExisting() && Method.HEAD.equals(getMethod()) && (linkeds == null) && //
				(((links == null) && hasUpdateDate(getEntity())) || //
						((links != null) && hasUpdateDate(linkEntity)))) {
			computeLastModificationDate();
			if (getLastModification() != null) {
				getResponse().getHeaders().add("Last-Modified", DateWriter.write(getLastModification()));
			}
		}
	}

	private boolean hasUpdateDate(MetaDataEntity entity) {
		return (entity != null) && (entity.getMetadata().getBoolean(MetaDataEntity.METADATA_UPDATABLE) || //
				(entity.getMetadata().get("updateCol") != null)); //$NON-NLS-1$
	}

	private void computeLastModificationDate() {
		final Form form = getRequestForm();
		MetaDataEntity entity = getEntity();
		final BeanMapList result;
		if (links != null) {
			int page = getFirst(form);
			int limit = getPageCount(form, page);
			// check other parameters...
			final boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
			final boolean distinct = isParameter(form, "distincts"); //$NON-NLS-1$
			final boolean ignoresubdivision = isParameter(form, "norec"); //$NON-NLS-1$
			ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
			if (ConstantCriteria.TRUE.equals(criteria) || (criteria == null)) {
				criteria = getLinksRightList();
			} else {
				final ISearchCriteria rc = getLinksRightList();
				if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
					criteria = new AndCriteria(criteria, rc);
				}
			}
			result = new BeanMapPartialList();
			for (final BeanMap item : getItems()) {
				final int count = getEntity().getMapper().linkCount(links, item.getId(), deleted, criteria, distinct,
						ignoresubdivision, getUser());
				// select when needed. (fusion of results).
				if ((page < count) && (limit != 0)) {
					// Select...
					final BeanMapList list = getEntity().getMapper().linkSelection(links, item.getId(),
							new ArrayList<ReferenceLine>(), deleted, criteria, distinct, ignoresubdivision, null,
							getUser(), page, limit);
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
				final ISearchCriteria rc = entity.getRightRead();
				if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
					criteria = new AndCriteria(criteria, rc);
				}
			}
			final OrCriteria or = new OrCriteria();
			for (final BeanMap item : getItems()) {
				or.add(new IdEqualCriteria(item.getId()));
			}
			if (criteria instanceof AndCriteria) {
				((AndCriteria) criteria).add(or);
			} else {
				criteria = new AndCriteria(criteria, or);
			}
			// Get the result offset.
			final int first = getFirst(form);
			final int number = getPageCount(form, first);
			// check other parameters...
			final boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
			final boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
			// Execution de la sélection proprement dite.
			result = entity.dataSelection(new ArrayList<ReferenceLine>(), deleted, criteria, distincts, null, getUser(),
					first, number);
		}
		Date date = new Date(0l);
		// Return EPOCH for empty results...
		if (result.size() > 0) {
			for (final IMetaDataSelectionListener listener : Activator.getInstance()
					.getSelectionListener(entity.getType())) {
				listener.onSelection(entity, result, getUser(), Language.ENGLISH_US);
			}
			for (final BeanMap b : result) {
				final Date d = b.getDate();
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
			final MetaDataEntity e = attLine.getLastAttribute().getRefEntity();
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
		if ((codes != null) && !codes.isBlank()) {
			links = new ArrayList<>();
			final String[] s = codes.split("/"); //$NON-NLS-1$
			if (s.length == 1) {
				getEntity().getLinkChain(links, s);
				if (links.isEmpty()) {
					links = null;
				} else {
					linkEntity = links.get(links.size() - 1).getRefEntity();
					linkeds = getLinksFromForm(getRequestForm());
				}
			} else {
				// The last code could be an id, a "code" value, a sum of these, or a link-code !
				MetaDataEntity e = getEntity();
				for (int i = 0; i < (s.length - 1); i++) {
					e.getLinkChain(links, s[i]);
					// The first link code must match at least one link code. (if not we are sure that this is not a link chain...)
					// the following code can be bad one we ignore them...
					if (links.isEmpty()) {
						links = null;
						return;
					}
					e = links.get(links.size() - 1).getRefEntity();
				}
				if (links.isEmpty()) {
					// The link chain does not match any link... let check if it is not and attribute line...
					links = null;
				} else {
					// Process separately the last code.
					final String lastCode = s[s.length - 1];
					// It may be a link code too.
					if (e.getLink(lastCode) != null) {
						e.getLinkChain(links, lastCode);
						linkEntity = links.get(links.size() - 1).getRefEntity();
						linkeds = getLinksFromForm(getRequestForm());
					} else {
						// Or one or many targeted entity id or "code" values. 
						linkEntity = e;
						linkeds = getLinksFromForm(getRequestForm());
						// Legacy: IDs and codes can be separated with spaces instead of + between identifiers.
						if (linkeds == null) {
							linkeds = new ArrayList<>();
						}
						final MetaDataAttribute att = getCodeAttribute(linkEntity);
						ReferenceLine refline = null;
						if (att != null) {
							refline = new ReferenceLine(att);
						}
						for (final String c : lastCode.replace(' ', '+').split("\\+")) { //$NON-NLS-1$
							if (!c.isBlank()) {
								BeanMap bean = null;
								try {
									bean = linkEntity.getMapper().selection(linkEntity, Integer.parseInt(c),
											(List<ReferenceLine>) null, true);
								} catch (final NumberFormatException ex) {
								}
								if ((bean == null) && (refline != null)) {
									bean = linkEntity.getMapper().selectionFirst(linkEntity, null, true, refline,
											Reference.decode(c));
								}
								if (bean != null) {
									linkeds.add(bean);
								}
							}
						}
						if (linkeds.isEmpty()) {
							linkeds = null;
						}
					}
				}
			}
		}
	}

	private void switchEntity(MetaDataEntity e) {
		final BeanMapList list = new BeanMapList();
		final String code = attLine.getCode();
		for (final BeanMap bean : getItems()) {
			final int id = bean.getInt(code);
			if (id > 0) {
				list.addUnique(e.getMapper().selection(e, id, (String) null, false));
			}
		}
		setItems(list);
		setEntity(e);
	}

	@Override
	protected List<ReferenceLine> getPreloadedAttributes() {
		if (codes != null) {
			// Manage the reference line to another targeted Entity:
			attLine = getEntity().getReferenceLine(Reference.decode(codes).replace('/', '.'));
			if (attLine != null) {
				if ((attLine.size() == 0) || (!attLine.isAttributeList()) || attLine.isFinal() || attLine.isHidden()) {
					// TODO We should test access right on the final attribute here...
					attLine = null;
					return null;
				}
				final ArrayList<ReferenceLine> list = new ArrayList<>(1);
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
		final Language language = getClientPreferedLanguage();
		final Form form = getRequestForm();
		if (linkeds != null) {
			// Delete the targeted links
			return deleteLinks(variant, form, language);
		}
		if (links != null) {
			// Deleted all links (or the ones corresponding the condition).
			return deleteAllLinks(variant, form, language);
		}
		final MetaDataEntity entity = getEntity();
		if (entity.isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		final boolean hardelete = isParameter(form, PARAM_HARDDELETE);
		boolean noerrors = true;
		final List<IMetaDataDeleteListener> listeners = Activator.getInstance().getDeleteListener(entity.getType());
		for (final BeanMap item : getItems()) {
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
				for (final IMetaDataDeleteListener listener : listeners) {
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
				for (final IMetaDataDeleteListener listener : listeners) {
					listener.postDeletion(entity, item, getUser(), language);
				}
				Activator.getInstance().fireDeleteEvent(entity, item, getUser(), hardelete ||
						(entity.getMetadata().get("deleteCol") == null)); //$NON-NLS-1$
			}
		}
		if (noerrors) {
			setStatus(Status.SUCCESS_NO_CONTENT);
		}
		return null;
	}

	/**
	 * Delete All Links. Optionnaly delete also linked items.
	 *
	 * @param variant
	 * @param form
	 * @param language
	 * @return
	 * @throws ResourceException
	 */
	private Representation deleteAllLinks(Variant variant, Form form, Language language) throws ResourceException {
		if (links.size() > 1) {
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED, Activator.getMessage("error.multilinkdelete", language)); //$NON-NLS-1$
		}
		final MetaDataLink link = links.get(0);
		final List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean deleted = false;
		final boolean hardelete = isParameter(form, PARAM_HARDDELETE);
		final boolean deleteitems = isParameter(form, PARAM_DELETETARGETS);
		final ISearchCriteria criteria = DataParentResource.getCriteria(this, form);
		for (final BeanMap item : getItems()) {
			// select linked items (Access right is tested below...)
			final BeanMapList linkedItems = getEntity().getMapper().linkSelection(link, item.getId(), new ArrayList<>(), false, criteria, false, null, getUser(), 0, -1);
			if (linkedItems != null) {
				for (final BeanMap linkedItem : linkedItems) {
					if (!linkEntity.getMapper().test(linkedItem, link.getRightCreate(true), getUser())) {
						// TODO The association rights tests should also be able to be applied at the source.
						errors = true;
					} else {
						boolean doit = true;
						boolean byPass = false;
						for (final IMetaDataLinkingListener listener : listeners) {
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
									// Check if cascade deletion also removed the link.
									if (!getEntity().getMapper().linkTest(link, item.getId(), linkedItem.getId())) {
										fireEvent = true;
									}
								}
							}
							if ((!byPass)
									&& getEntity().getMapper().linkRemove(link, item.getId(), linkedItem.getId())) {
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
		final Language language = getClientPreferedLanguage();
		final Form form = getRequestForm();
		if (linkeds != null) {
			return testLinks(variant, form, language);
		}
		if (links != null) {
			return listLinks(variant, form, language);
		}
		final MetaDataEntity entity = getEntity();
		// Build the requested attribute list.
		final List<ReferenceLine> attributes = getSelectedAttributes(getEntity(), false);
		// Build the requested sort order list.
		final List<ReferenceLine> orders;
		if (attributes.size() == 0) {
			orders = new ArrayList<>();
		} else {
			orders = entity.getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
		}
		final Iterator<ReferenceLine> itt = orders.iterator();
		while (itt.hasNext()) {
			final ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				final MetaDataAttribute a = att.getLastAttribute();
				if (a.getRightRead(false) != null) {
					// TODO This test should be supplemented by a test on the joint.
					final MetaDataEntity e = a.getParent();
					if (!e.getMapper().test(e, a.getRightRead(false), getUser())) {
						itt.remove();
						continue;
					}
				}
				// If the column is not selected it is added.
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
			final ISearchCriteria rc = entity.getRightRead();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		final OrCriteria or = new OrCriteria();
		for (final BeanMap item : getItems()) {
			or.add(new IdEqualCriteria(item.getId()));
		}
		if (criteria instanceof AndCriteria) {
			((AndCriteria) criteria).add(or);
		} else {
			criteria = new AndCriteria(criteria, or);
		}
		// Get the result offset.
		final int first = getFirst(form);
		final int number = getPageCount(form, first);
		// check other parameters...
		final boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		final boolean distincts = isParameter(form, "distincts"); //$NON-NLS-1$
		final boolean translate = !isParameter(form, "notranslation"); //$NON-NLS-1$
		// Add all ".code" required for "translate" items...
		if (translate) {
			for (final ReferenceLine rl : attributes) {
				if (rl.isTranslatable()) {
					final ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !attributes.contains(trl)) {
						attributes.add(trl);
					}
				}
			}
		}
		// Execution of the selection itself.
		final BeanMapList result = entity.dataSelection(attributes, deleted, criteria, distincts, orders, getUser(),
				first, number);
		if (translate) {
			for (final ReferenceLine rl : attributes) {
				if (rl.isTranslatable()) {
					for (final BeanMap bm : result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		// The "read" test has no direct influence on the further processing (it can only modify the result):
		Activator.getInstance().test(MetaDataTest.EVENTCODE_READ, entity, result, getUser(), language);
		if ((entity.getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) && (result.size() > 1)) {
			Activator.getInstance().fireSelectionEvent(getEntity(), result, getUser());
		}
		if (result.size() > 0) {
			for (final IMetaDataSelectionListener listener : //
					Activator.getInstance().getSelectionListener(getEntity().getType())) { //
				listener.onSelection(getEntity(), result, getUser(), language);
			}
		}
		// formatting the result:
		switch (result.size()) {
		case 0:
			// Generate an error message only if you are SURE that the result is empty because the
			// access right test failed. (the list could be empty because the table is empty...)
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
			Representation er = postProcessConditionalHeaders(result);
			if (er != null) {
				return er;
			}
			return getRepresentation(variant, form, result.get(0), language, false);
		default:
			er = postProcessConditionalHeaders(result);
			if (er != null) {
				return er;
			}
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
				} catch (final NumberFormatException e) {
				}
			}
		} else {
			try {
				return Integer.parseInt(s);
			} catch (final NumberFormatException e) {
			}
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
			} catch (final NumberFormatException e) {
			}
		}
		return 0;
	}

	@Override
	protected Representation put(Representation representation, Variant variant) throws ResourceException {
		if (!isExisting()) {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
			return null;
		}
		final Language language = getClientPreferedLanguage();
		final Form form = getRequestForm();
		if (links != null) {
			return addLinks(variant, form, language);
		}
		final MetaDataEntity entity = getEntity();
		if (entity.isReadOnly()) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("error.readonly", language)); //$NON-NLS-1$
			return null;
		}
		// Set a default status.
		setStatus(Status.SUCCESS_OK);
		final ArrayList<MetaDataAttribute> attlist = new ArrayList<>();
		final BeanMap values = entity.formToBean(form, attlist);
		final List<IMetaDataModifyListener> listeners = Activator.getInstance().getModifyListener(getType());
		List<IMetaDataUndeleteListener> udlisteners = null;
		final BeanMapList list = new BeanMapList(getItems().size());
		final boolean undelete = isParameter(form, "undelete"); //$NON-NLS-1$
		if (undelete) {
			udlisteners = Activator.getInstance().getUndeleteListener(getType());
			// Undelete is not an attribute...
			values.remove("undelete"); //$NON-NLS-1$
		}
		for (final BeanMap item : getItems()) {
			final BeanMap result = put(variant, form, entity, item, values, attlist, language, listeners, udlisteners);
			if (result != null) {
				broadcastUserAction("logUpdate", result); //$NON-NLS-1$
				list.add(result);
			}
		}
		if (Status.SUCCESS_OK.equals(getStatus())) {
			// If there is some errors we do not have to generate a result even if some of the modification can be
			// completed.
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
			ArrayList<MetaDataAttribute> attlist, Language language, List<IMetaDataModifyListener> listeners,
			List<IMetaDataUndeleteListener> undeleteListeners) {
		if (!hasRightUpdate(entity, item, undeleteListeners != null, true, language)) {
			setStatus(Status.CLIENT_ERROR_FORBIDDEN, Activator.getMessage("right.noupdate", language)); //$NON-NLS-1$
			return null;
		}
		// Support undelete !
		if (undeleteListeners != null) {
			if (!item.isDeleted()) {
				// Just log a message, this operation is idempotent !
				Activator.getInstance().warn(Messages.Undelete_Undeleted + item.getType() + '/' + item.getId());
			}
			switch (doUndeleteTest(undeleteListeners, item, values, attlist, language)) {
			case 0:
				getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST,
						Activator.getMessage("error.undeletionfail", language)); //$NON-NLS-1$
				return null;
			case 1:
				// Check that the undeleted values go against an "unique value" constraint...
				for (final MetaDataAttribute attribute : entity.getAttributes().values()) {
					if (attribute.getMetadata().getBoolean(MetaDataEntity.METADATA_UNIQUE)) {
						Object value = values.get(attribute.getCode());
						if (value == null) {
							value = item.get(attribute.getCode());
						}
						if ((value != null) && (value.toString().length() > 0) && // ignore null values...
								(getEntity().dataCount(false, attribute.getCode(), value) > 0)) {
							throw new ResourceException(Status.CLIENT_ERROR_PRECONDITION_FAILED, //
									String.format(Activator.getMessage("error.uniqueattribute.undelete", language), //$NON-NLS-1$
											attribute.getName(language), value, attribute.getCode()));
						}
					}
				}
				entity.getMapper().undelete(item);
			case -1: // bypass...
				item.setDeleted(false);
			}
			for (final IMetaDataUndeleteListener listener : undeleteListeners) {
				listener.postUndeletion(entity, item, getUser(), language);
			}
			Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERUNDELETE, entity, item, values, attlist, getUser(),
					language);
			Activator.getInstance().fireUndeleteEvent(getEntity(), item, getUser());
		}
		// attlist must not be changed (it is used by others updates...)
		final ArrayList<MetaDataAttribute> attlistex = new ArrayList<>(attlist.size());
		for (final MetaDataAttribute att : attlist) {
			if ((att.getRightUpdate(false) == null) || //
					entity.getMapper().test(entity, item.getId(), att.getRightUpdate(false), getUser())) {
				attlistex.add(att);
			} else {
				item.remove(att.getCode());
			}
		}
		switch (doUpdateTest(listeners, item, values, attlistex, language)) {
		case 0:
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST,
					Activator.getMessage("error.missingattributes", language)); //$NON-NLS-1$
			return null;
		case -1:
			values.forceId(item.getId());
			values.setDeleted(item.isDeleted());
			break;
		case 1:
			// Check "unique" values constraints
			if (undeleteListeners == null) {
				// Already done if undelete is true (see above !)
				final ISearchCriteria invariantCriteria = new NotCriteria(new IdEqualCriteria(item.getId()));
				for (final MetaDataAttribute attribute : attlist) {
					if (attribute.getMetadata().getBoolean(MetaDataEntity.METADATA_UNIQUE)) {
						final Object value = values.get(attribute.getCode());
						if ((value != null) && (value.toString().length() > 0)) { // ignore null values...
							// Removes the current element from the selection (avoids updates on the same value)
							final EqualCriteria codeEqual = new EqualCriteria();
							codeEqual.setAttribute(attribute.getCode());
							if (MetaDataAttribute.TYPE_INTEGER.equals(attribute.getType())
									|| MetaDataAttribute.TYPE_INT.equals(attribute.getType())) {
								try {
									codeEqual.setIntval(Integer.valueOf(value.toString()));
								} catch (final NumberFormatException e) {
									codeEqual.setValue(value.toString());
								}
							} else {
								codeEqual.setValue(value.toString());
							}
							if (getEntity().dataCount(false, new AndCriteria(invariantCriteria, codeEqual), false,
									getUser()) > 0) {
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
			getResponse().setStatus(Status.CLIENT_ERROR_BAD_REQUEST,
					Activator.getMessage("error.badattributes", language)); //$NON-NLS-1$
			return null;
		}
		doPostUpdateTreatment(listeners, item, values, attlistex, language);
		return values;
	}

	private int doUpdateTest(List<IMetaDataModifyListener> listeners, BeanMap oldValue, BeanMap result,
			ArrayList<MetaDataAttribute> list, Language language) {
		final MetaDataEntity entity = getEntity();
		boolean bypass = false;
		for (final IMetaDataModifyListener listener : listeners) {
			if (!listener.testModification(entity, oldValue, result, list, getUser(), language)) {
				return 0;
			}
			if (listener instanceof IByPassListener) {
				bypass = true;
			}
		}
		if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREUPDATE, entity, oldValue, result, list,
				getUser(), language)) {
			return 0;
		}
		if (bypass) {
			return -1;
		}
		return 1;
	}

	private int doUndeleteTest(List<IMetaDataUndeleteListener> listeners, BeanMap oldValue, BeanMap result,
			ArrayList<MetaDataAttribute> list, Language language) {
		final MetaDataEntity entity = getEntity();
		boolean bypass = false;
		for (final IMetaDataUndeleteListener listener : listeners) {
			if (!listener.testUndeletion(entity, oldValue, getUser(), language)) {
				return 0;
			}
			if (listener instanceof IByPassListener) {
				bypass = true;
			}
		}
		if (!Activator.getInstance().test(MetaDataTest.EVENTCODE_BEFOREUNDELETE, entity, oldValue, result, list,
				getUser(), language)) {
			return 0;
		}
		if (bypass) {
			return -1;
		}
		return 1;
	}

	private void doPostUpdateTreatment(List<IMetaDataModifyListener> listeners, BeanMap oldValue, BeanMap result,
			ArrayList<MetaDataAttribute> list, Language language) {
		// TODO translate !?!??!! (en théorie les attributs translate ne sont pas modifiés !
		final MetaDataEntity entity = getEntity();
		for (final IMetaDataModifyListener listener : listeners) {
			listener.postModification(entity, oldValue, result, list, getUser(), language);
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_AFTERUPDATE, entity, oldValue, result, list, getUser(),
				language);
		Activator.getInstance().fireUpdateEvent(entity, oldValue, result, getUser());
		for (final MetaDataAttribute att : entity.getAttributes().values()) {
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
		if (links.size() > 1) {
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED, Activator.getMessage("error.multilinkdelete", language)); //$NON-NLS-1$
		}
		if ((linkeds == null) || linkeds.isEmpty()) {
			setStatus(Status.CLIENT_ERROR_NOT_FOUND);
			return null;
		}
		final MetaDataLink link = links.get(0);
		final List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean deleted = false;
		for (final BeanMap item : getItems()) {
			for (final BeanMap linked : linkeds) {
				if (!linkEntity.getMapper().test(linked, link.getRightCreate(true), getUser())) {
					// TODO The association rights tests should also be able to be applied at the source!!!
					errors = true;
				} else {
					boolean doit = true;
					boolean bypass = false;
					for (final IMetaDataLinkingListener listener : listeners) {
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
		// List an association, supports multi-id and pagination.
		List<ReferenceLine> attributes = getSelectedAttributes(linkEntity, true);
		// Get the result offset.
		int page = getFirst(form);
		int limit = getPageCount(form, page);
		// Check other parameters...
		final boolean deleted = isParameter(form, "deleted"); //$NON-NLS-1$
		final boolean distinct = isParameter(form, "distincts"); //$NON-NLS-1$
		final boolean translate = !isParameter(form, "notranslation"); //$NON-NLS-1$
		final boolean ignoresubdivision = isParameter(form, "norec"); //$NON-NLS-1$
		final List<ReferenceLine> orders = linkEntity.getPublicAttributeLines(getColumns(form, "orders")); //$NON-NLS-1$
		final Iterator<ReferenceLine> itt = orders.iterator();
		while (itt.hasNext()) {
			final ReferenceLine att = itt.next();
			if (att.isEmpty()) {
				itt.remove();
			} else {
				final MetaDataAttribute a = att.getLastAttribute();
				if (a.getRightRead(false) != null) {
					// TODO Ce test devrait être complété par un test sur la jointure !
					final MetaDataEntity e = a.getParent();
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
			criteria = getLinksRightList();
		} else {
			final ISearchCriteria rc = getLinksRightList();
			if ((rc != null) && !ConstantCriteria.TRUE.equals(rc)) {
				criteria = new AndCriteria(criteria, rc);
			}
		}
		if (translate) {
			final List<ReferenceLine> allattributes = new ArrayList<>(attributes);
			for (final ReferenceLine rl : attributes) {
				if (rl.isTranslatable()) {
					final ReferenceLine trl = rl.getTranslateCode();
					if ((trl != null) && !allattributes.contains(trl)) {
						allattributes.add(trl);
					}
				}
			}
			attributes = allattributes;
		}
		final BeanMapPartialList result = new BeanMapPartialList();
		for (final BeanMap item : getItems()) {
			final int count = getEntity().getMapper().linkCount(links, item.getId(), deleted, criteria, distinct,
					ignoresubdivision, getUser());
			result.incrementTotal(count);
			// select when needed. (fusion of results).
			if ((page < count) && (limit != 0)) {
				// If we are starting to load the result list, we set the rank.
				if (result.size() == 0) {
					result.setRank((result.getTotal() - count) + page);
				}
				// Select...
				final BeanMapList list = getEntity().getMapper().linkSelection(links, item.getId(), attributes, deleted,
						criteria, distinct, ignoresubdivision, orders, getUser(), page, limit);
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
			for (final ReferenceLine rl : attributes) {
				if (rl.isTranslatable()) {
					for (final BeanMap bm : result) {
						rl.translate(bm, language);
					}
				}
			}
		}
		Activator.getInstance().test(MetaDataTest.EVENTCODE_LIST, linkEntity, result, getUser(), language);
		if (getEntity().getMetadata().getBoolean(MetaDataEntity.METADATA_EVENTONSELECTION)) {
			Activator.getInstance().fireSelectionEvent(linkEntity, result, getUser());
		}
		for (final IMetaDataSelectionListener listener : //
				Activator.getInstance().getSelectionListener(linkEntity.getType())) {
			listener.onSelection(linkEntity, result, getUser(), language);
		}
		final Representation er = postProcessConditionalHeaders(result);
		if (er != null) {
			return er;
		}
		return getRepresentation(variant, form, result, language, true);
	}

	private ISearchCriteria getLinksRightList() {
		if (links == null) {
			return null;
		}
		final AndCriteria and = new AndCriteria();
		// As we are not going to list the entities in the middle of the list, we do not need to check their access
		// rights.
		for (final MetaDataLink l : links) {
			and.add(l.getRightList(false));
		}
		and.add(links.get(links.size() - 1).getRefEntity().getRightList());
		if (and.isEmpty()) {
			return null;
		}
		if (and.getCriterias().size() == 1) {
			return and.getCriterias().get(0);
		}
		return and;
	}

	private Representation testLinks(Variant variant, Form form, Language language) {
		if (linkeds == null) {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.missinglinkrefs", language)); //$NON-NLS-1$
			return null;
		}
		final boolean ignoresubdivision = isParameter(form, "norec"); //$NON-NLS-1$
		boolean tested = false;
		for (final BeanMap item : getItems()) {
			for (final BeanMap linked : linkeds) {
				if (getEntity().getMapper().linkTest(links, item.getId(), linked.getId(), ignoresubdivision)) {
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
		if (links.size() > 1) {
			throw new ResourceException(Status.SERVER_ERROR_NOT_IMPLEMENTED, Activator.getMessage("error.multilinkcreation", language)); //$NON-NLS-1$
		}
		if (linkeds == null) {
			setStatus(Status.CLIENT_ERROR_BAD_REQUEST, Activator.getMessage("error.missinglinkrefs", language)); //$NON-NLS-1$
			return null;
		}
		final MetaDataLink link = links.get(0);
		final List<IMetaDataLinkingListener> listeners = Activator.getInstance().getLinkingListener(link);
		boolean errors = false;
		boolean added = false;
		for (final BeanMap item : getItems()) {
			for (final BeanMap linked : linkeds) {
				if (getEntity().getMapper().linkTest(link, item.getId(), linked.getId())) {
					// If the data are already linked we ignore this operation, but we return an "Ok" message.
					Activator.getInstance().debug(Messages.Link_Exists + link.getParent().getType() + //
							'.' + link.getCode() + " [" + item.getId() + " -> " + linked.getId() + ']'); //$NON-NLS-1$ //$NON-NLS-2$
					added = true;
				} else if (!linkEntity.getMapper().test(linked, link.getRightCreate(true), getUser())) {
					// TODO The association rights tests should also be able to be applied at the source!!!
					errors = true;
				} else {
					boolean doit = true;
					for (final IMetaDataLinkingListener listener : listeners) {
						if (!listener.testLink(link, item, linked, getUser(), language)) {
							doit = false;
							errors = true;
							break;
						}
					}
					// FIXME If the attribute of a reverse-link is also declared as unique then its new value must be tested.
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
		final ArrayList<BeanMap> result = new ArrayList<>(ids.length);
		for (final String id : ids) {
			if (id.length() > 0) {
				BeanMap bean = null;
				try {
					bean = linkEntity.getMapper().selection(linkEntity, Integer.parseInt(id),
							(List<ReferenceLine>) null, true);
				} catch (final NumberFormatException e) {
				}
				if (bean != null) {
					result.add(bean);
				}
			}
		}
		ids = form.getValuesArray(ATTRIBUTE_CODE, true);
		if ((ids != null) && (ids.length > 0)) {
			final MetaDataAttribute att = linkEntity.getAttribute(ATTRIBUTE_CODE);
			ReferenceLine refline = null;
			if (att != null) {
				refline = new ReferenceLine(att);
			}
			for (final String code : ids) {
				if (code.length() > 0) {
					final BeanMap bean = linkEntity.getMapper().selectionFirst(linkEntity, null, true, refline, code);
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
