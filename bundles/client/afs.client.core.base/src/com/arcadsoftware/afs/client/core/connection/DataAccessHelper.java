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
package com.arcadsoftware.afs.client.core.connection;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.Map;
import java.util.Properties;

import org.restlet.data.Method;
import org.restlet.representation.Representation;

import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.afs.framework.messages.UserMessageManager;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.BeanMapPartialList;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.client.DataAccess;
import com.arcadsoftware.metadata.criteria.ISearchCriteria;
import com.arcadsoftware.rest.ServerErrorException;

/**
 * 
 * 
 * @author ARCAD Software
 * @deprecated This class does not return the actual error avoiding to present the error message to the end user which is a bad practice. Use the DataAccess class instead of this one.
 */
public class DataAccessHelper {

	private static final String CONFIG_TYPE = "config";

	private ServerConnection connection;
	protected DataAccess dataAccess;
	private UserMessage lastMessage;
	protected Throwable lastCause;

	public DataAccessHelper(ServerConnection connection) {
		this.connection = connection;
		// FIXME Si le ServerConnection change de DataAccess on n'est pas mis au courant !!!!!!
		dataAccess = connection.getDataAccess();
	}

	private void resetXStream() {
		dataAccess.getWebServicesAccess().setXStream(new XmlBeanMapStream());
	}

	public MetaDataEntity getEntity(String type) {
		try {
			return dataAccess.getEntity(type);

		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public String getLayoutFile(String name, String type) {
		try {
			return dataAccess.getLayoutFile(name, type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public Properties getProperties(String fileKey) {
		try {
			return dataAccess.getProperties(fileKey);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMapList getList(String type) {
		try {
			return dataAccess.getList(type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, String attributes) {
		try {
			return dataAccess.getList(type, attributes, (ISearchCriteria) null, null, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, String attributes, String orders) {
		try {
			return dataAccess.getList(type, attributes, (ISearchCriteria) null, orders, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, String attributes, ISearchCriteria criteria, String orders) {
		try {
			return dataAccess.getList(type, attributes, criteria, orders, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, String attributes,
			ISearchCriteria criteria, String orders, int page, int count, boolean deleted) {
		try {
			return dataAccess.getList(type, attributes, criteria, orders, page, count, deleted);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, ISearchCriteria criteria) {
		try {
			return dataAccess.getList(type, null, criteria, null, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, ISearchCriteria criteria, String orders) {
		try {
			return dataAccess.getList(type, null, criteria, orders, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getList(String type, String attributes, ISearchCriteria criteria) {
		try {
			return dataAccess.getList(type, attributes, criteria, null, 0, -1, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMap getFirst(String type, ISearchCriteria criteria) {
		try {
			final BeanMapList l = dataAccess.getList(type, null, criteria, null, 0, -1, false);
			if (l.size() > 0) {
				return l.get(0);
			}
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMapList getList(String type, String attributes,
			String criteria, String orders, int page, int count, boolean deleted) {
		try {
			return dataAccess.getList(type, attributes, criteria, orders, page, count, deleted, true);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getListFromPath(String path, String type, String attributes,
			String criteria, String orders, int page, int count, boolean deleted) {
		try {
			return dataAccess.getList(path, type, attributes, criteria, orders, page, count, deleted, true);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getListFromPath(String path, String type) {
		try {
			return dataAccess.getList(path, type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	public BeanMapList getListFromPath(String path, String type, BeanMap parameters) {
		try {
			return dataAccess.getList(path, type, parameters);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	private String createAttributeParameters(String baseAddress, String attributeList) {
		if (attributeList != null) {
			final StringBuilder attributesParameter = new StringBuilder("");
			final String[] attributes = attributeList.split(" ");
			if (attributes.length > 0) {
				for (final String att : attributes) {
					if (att.length() > 0) {
						attributesParameter.append(att).append("+");
					}
				}
				if (attributesParameter.length() > 0) {
					final StringBuilder additionalParameters = new StringBuilder("?attributes=");
					// We remove the last '+'
					additionalParameters.append(attributesParameter.substring(0, attributesParameter.length() - 1));
					baseAddress = baseAddress + additionalParameters.toString();
				}
			}
		}
		return baseAddress;
	}

	/**
	 * Return a list of beanmap linked to the master beanMap by the association called associationKey
	 *
	 * @param type
	 *            The type of the master beanMap
	 * @param id
	 *            the id of the master bean map
	 * @param associationKey
	 *            the Association Name
	 * @param linkedType
	 *            The type of the returned Bean Map
	 * @return
	 */
	public BeanMapList getLinkList(String type, int id, String associationKey, String linkedType) {
		return getLinkList(type, id, associationKey, linkedType, null);
	}

	public BeanMapList getLinkList(String type, int id, String associationKey, String linkedType, String attributeList,
			String orderList, int count) {
		try {
			associationKey = createAttributeParameters(associationKey, attributeList);
			final BeanMap bean = new BeanMap(type, id);
			return dataAccess.getLinks(bean, associationKey, linkedType, attributeList, null, orderList, 0, count,
					false);

		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	/**
	 * Return count of beanmaps linked to the master beanMap (type, id) by the association called associationKey
	 *
	 * @param type
	 *            The type of the master beanMap
	 * @param id
	 *            the id of the master bean map
	 * @param associationKey
	 *            the Association Name
	 * @param linkedType
	 *            The type of the returned Bean Map
	 * @return
	 */
	public int getCountLinkList(String type, int id, String associationKey, String linkedType) {
		final BeanMapList list = getLinkList(type, id, associationKey, linkedType, null);
		if (list == null) {
			return 0;
		} else {
			return list.size();
		}
	}

	public BeanMapList getLinkList(String type, int id, String associationKey, String linkedType,
			String attributeList) {
		return getLinkList(type, id, associationKey, linkedType, attributeList, null);
	}

	/**
	 * Return a list of beanmap linked to the master beanMap by the association called associationKey
	 *
	 * @param type
	 *            The type of the master beanMap
	 * @param id
	 *            the id of the master bean map
	 * @param associationKey
	 *            the Association Name
	 * @param linkedType
	 *            The type of the returned Bean Map
	 * @param attributeList
	 *            blank-separated list of attributes
	 * @param orderList
	 *            blank-separated list of attributes used to order the list
	 * @return
	 */
	public BeanMapList getLinkList(String type, int id, String associationKey, String linkedType, String attributeList,
			String orderList) {
		try {
			associationKey = createAttributeParameters(associationKey, attributeList);
			final BeanMap bean = new BeanMap(type, id);
			return dataAccess.getLinks(bean, associationKey, linkedType, attributeList, null, orderList, 0, -1, false);

		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	/**
	 * Return a list of beanmap linked to the master beanMap by the association called associationKey
	 *
	 * @param type
	 *            The type of the master beanMap
	 * @param id
	 *            the id of the master bean map
	 * @param associationKey
	 *            the Association Name
	 * @param linkedType
	 *            The type of the returned Bean Map
	 * @param attributeList
	 *            blank-separated list of attributes
	 * @param orderList
	 *            blank-separated list of attributes used to order the list
	 * @param criteria
	 *            criteria of selection
	 * @return
	 */
	public BeanMapList getLinkList(String type, int id, String associationKey, String linkedType, String attributeList,
			String orderList, String criteria) {
		try {
			associationKey = createAttributeParameters(associationKey, attributeList);
			final BeanMap bean = new BeanMap(type, id);
			return dataAccess.getLinks(bean, associationKey, linkedType, attributeList, criteria, orderList, 0, -1,
					false);

		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return new BeanMapList();
	}

	/**
	 * Return a list of beanmap linked to the master beanMap by the association called associationKey
	 *
	 * @param type
	 *            The type of the master beanMap
	 * @param id
	 *            the id of the master bean map
	 * @param associationKey
	 *            the Association Name
	 * @param linkedType
	 *            The type of the returned Bean Map
	 * @return
	 */
	public BeanMapList getLinkList(BeanMap master, String associationKey, String linkedType) {
		return getLinkList(master.getType(), master.getId(), associationKey, linkedType, null);
	}
	
	/**
	 * 
	 * @param beanmap
	 * @return
	 */
	public boolean create(BeanMap beanmap) {
		try {
			final BeanMap b = dataAccess.post(beanmap);
			if (b != null) {
				beanmap.setId(b.getId());
				// FIXME the values of the original beanmap are not correctly cleared !
				beanmap.addAll(b);
				return true;
			}
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Create beanMap from file input
	 *
	 * @param url
	 * @param type
	 * @param file
	 * @return
	 */
	public BeanMap create(String url, String type, File file) {
		try {
			return dataAccess.uploadToBeanMap(url, type, file);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public boolean createLink(String type, int id, String linkCode, int subid) {
		try {
			dataAccess.addLink(type, id, linkCode, subid);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * @param master
	 *            The master BeanMap
	 * @param linkCode
	 *            the link name used
	 * @param subid
	 *            the id of the bean map to link
	 * @return
	 */
	public boolean createLink(BeanMap master, String linkCode, int subid) {
		try {
			dataAccess.addLink(master.getType(), master.getId(), linkCode, subid);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Create set of links in one call to server
	 *
	 * @param master
	 *            Master BeanMap
	 * @param linkCode
	 *            link code to follow
	 * @param subids
	 *            list of linked Items ids
	 * @return
	 */
	public boolean createLinks(BeanMap master, String linkCode, int[] subids) {
		try {
			dataAccess.addLinks(master.getType(), master.getId(), linkCode, subids);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * @param beanmap
	 * @return
	 */
	public boolean delete(BeanMap beanmap) {
		try {
			dataAccess.delete(beanmap);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean delete(String type, int id) {
		try {
			dataAccess.delete(type, id);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean delete(String type, ISearchCriteria criteria) {
		try {
			dataAccess.delete(type, criteria);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean remove(String type, ISearchCriteria criteria) {
		try {
			dataAccess.deleteHard(type, criteria);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean deleteLink(String type, int id, String linkCode, int subId) {
		try {
			dataAccess.removeLink(type, id, linkCode, subId);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Remove associated links to an item
	 *
	 * @param type
	 * @param id
	 * @param linkCode
	 * @return
	 */
	public boolean removeLinks(String type, int id, String linkCode) {
		try {
			dataAccess.removeLinks(type, id, linkCode);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Remove associated links to an item
	 *
	 * @param type
	 *            Master type
	 * @param id
	 *            Master Id
	 * @param linkCode
	 *            Link code
	 * @param deleteLinkedItems
	 *            True if linked items must also be deleted. If False, only links will be removed
	 * @return
	 */
	public boolean removeLinks(String type, int id, String linkCode, boolean deleteLinkedItems) {
		try {
			dataAccess.removeLinkedItems(type, id, linkCode, deleteLinkedItems);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean remove(BeanMap beanmap) {
		try {
			dataAccess.remove(beanmap);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean remove(String type, int id) {
		try {
			dataAccess.remove(type, id);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean update(BeanMap beanmap) {
		try {
			dataAccess.put(beanmap);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public BeanMap read(BeanMap beanmap) {
		try {
			return dataAccess.get(beanmap);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap read(String type, int id) {
		try {
			return dataAccess.get(type, id);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap read(String type, String code) {
		try {
			return dataAccess.get(type, code, false);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public void refresh(BeanMap b) {
		try {
			final BeanMap res = dataAccess.get(b.getType(), b.getId());
			if (res != null) {
				b.addAll(res);
			}
		} catch (final ServerErrorException e) {
			manageException(e);
		}
	}

	public void refresh(BeanMap b, String attributeList) {
		try {
			final String address = createAttributeParameters("data/" + b.getType() + "/" + b.getId(), attributeList);
			final BeanMap res = dataAccess.get(address, b.getType());
			if (res != null) {
				b.addAll(res);
			}
		} catch (final ServerErrorException e) {
			manageException(e);
		}
	}

	public BeanMap readFromPath(String adress, String type) {
		try {
			return dataAccess.get(adress, type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap read(String type, int id, String attributeList) {
		try {
			final String address = createAttributeParameters("data/" + type + "/" + id, attributeList);
			return dataAccess.get(address, type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap read(String type, ISearchCriteria criteria) {
		final BeanMapList l = getList(type, criteria);
		if ((l != null) && (!l.isEmpty())) {
			return l.get(0);
		}
		return null;
	}

	public InputStream getBeanStream(String type, int id) {
		try {
			final Representation representation = dataAccess.getBeanStream(type, id);
			if (representation != null) {
				return representation.getStream();
			}
		} catch (ServerErrorException | IOException e) {
			manageException(e);
		}
		return null;
	}

	public InputStream getBeanStream(BeanMap beanMap) {
		try {
			final Representation representation = dataAccess.getBeanStream(beanMap.getType(), beanMap.getId());
			if (representation != null) {
				return representation.getStream();
			}
		} catch (ServerErrorException | IOException e) {
			manageException(e);
		}
		return null;
	}

	public boolean deleteBeanStream(BeanMap beanMap) {
		try {
			dataAccess.deleteBeanStream(beanMap.getType(), beanMap.getId());
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean uploadBeanStream(String type, int id, File file) {
		try {
			dataAccess.uploadBeanStream(type, id, file);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean uploadBeanStream(BeanMap beanMap, File file) {
		try {
			dataAccess.uploadBeanStream(beanMap.getType(), beanMap.getId(), file);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean uploadFile(int id, File file, String remotePath) {
		try {
			dataAccess.uploadImage(remotePath, id, file);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Simply Upload a file
	 *
	 * @param url
	 *            web-service able to manage/store/... that file
	 * @param file
	 * @return
	 */
	public boolean uploadFile(String url, File file) {
		try {
			dataAccess.upload(url, file);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Simply Upload a file
	 *
	 * @param url
	 *            web-service able to manage/store/... that file
	 * @param file
	 * @return
	 */
	public boolean uploadFile(String url, String remotefile, File file) {
		try {
			final BeanMap b = new BeanMap();
			b.put("path", remotefile);
			dataAccess.post(url, b, file);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Simply Upload a file
	 *
	 * @param url
	 *            web-service able to manage/store/... that file
	 * @param file
	 * @return
	 */
	public boolean uploadFile(String url, BeanMap params, InputStream inputStream) {
		try {
			dataAccess.post(url, params, inputStream);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Uploads a file and returns a BeanMap result of a given type.
	 */
	public BeanMap uploadFile(String url, BeanMap params, InputStream inputStream, String type) {
		try {
			return dataAccess.post(url, params, inputStream, type);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public String getUploadRedirection(String type, int id) {
		try {
			return dataAccess.getUploadRedirection(Method.POST, type, id);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public void manageException(Throwable e) {
		if (e instanceof ServerErrorException) {
			connection.manageErrorException((ServerErrorException) e);
			lastMessage = connection.getErrorMessage();
			lastCause = e.getCause();
		} else {
			lastMessage = UserMessageManager.getInstance().getMessage(ISRVMessages.ERR_COR_EXCEPTION, e);
		}
	}

	public UserMessage getLastMessage() {
		return lastMessage;
	}

	public Throwable getLastCause() {
		return lastCause;
	}

	public BeanMap post(String url, BeanMap request) {
		try {
			lastMessage = null;
			resetXStream();
			return dataAccess.post(url, request);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap post(String url, BeanMap request, String resultType) {
		try {
			lastMessage = null;
			return dataAccess.post(url, request, resultType);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public boolean put(String url, BeanMap request) {
		try {
			lastMessage = null;
			final BeanMap b = dataAccess.put(url, request);
			if (b != null) {
				request.setId(b.getId());
				request.addAll(b);
				return true;
			}
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean get(String url, BeanMap request) {
		try {
			lastMessage = null;
			final BeanMap result = dataAccess.get(url, request.getType());
			request.addAll(result);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public boolean delete(String url) {
		try {
			lastMessage = null;
			dataAccess.delete(url);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	/**
	 * Delete from url with parameters
	 *
	 * @param url
	 * @param params
	 */
	public boolean delete(String url, BeanMap params) {
		try {
			lastMessage = null;
			dataAccess.delete(url, params);
			return true;
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public String getXml(String url) {
		try {
			lastMessage = null;
			return dataAccess.getXml(url);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public InputStream getFileStream(String url) {
		try {
			lastMessage = null;
			return dataAccess.getWebServicesAccess().getSecuredRaw(url, (Date) null).getStream();
		} catch (ServerErrorException | IOException | NullPointerException e) {
			manageException(e);
		}
		return null;
	}

	public BeanMap getConfiguration(String pid) {
		try {
			lastMessage = null;
			return dataAccess.get("config/" + pid, CONFIG_TYPE);
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return null;
	}

	public void purgeCache(BeanMap bean) {
		dataAccess.purgeCache(bean);
	}

	public void purgeCache(String type, int id) {
		dataAccess.purgeCache(type, id);
	}

	public boolean exists(String type, ISearchCriteria criteria) {
		try {
			final BeanMapList l = dataAccess.getList(type, null, criteria, null, 0, -1, false);
			return !l.isEmpty();
		} catch (final ServerErrorException e) {
			manageException(e);
		}
		return false;
	}

	public String getRedirection(String path) {
		try {
			return dataAccess.getRedirection(Method.GET, path);
		} catch (final ServerErrorException e) {
			manageException(e);
			return null;
		}
	}

	public ServerConnection getConnection() {
		return connection;
	}

	public String rawGet(String url, Map<String, Object> parameters) {
		try {
			return dataAccess.getWebServicesAccess().get(url, parameters);
		} catch (final ServerErrorException e) {
			manageException(e);
			return null;
		}
	}

	public int count(String type, ISearchCriteria criteria) {
		try {
			BeanMapList list = dataAccess.getList(type, "", criteria, null, 0, 1, false);
			if (list instanceof BeanMapPartialList) {
				return ((BeanMapPartialList) list).getTotal();
			}
			return list.size();
		} catch (final ServerErrorException e) {
			manageException(e);
			return 0;
		}
	}

}
