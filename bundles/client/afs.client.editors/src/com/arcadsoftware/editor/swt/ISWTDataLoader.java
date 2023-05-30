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
package com.arcadsoftware.editor.swt;

import java.io.File;
import java.io.InputStream;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;

import com.arcadsoftware.afs.framework.messages.UserMessage;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.beanmap.IBeanMapListener;

/**
 * This loader is used to load the necessary information for SWT implementation of the dynamic Editor.
 */
public interface ISWTDataLoader {

	public void setDisplay(Display display);
	
	/**
	 * Load an image and return the corresponding ImageDescriptor;
	 * 
	 * <p>
	 * This load must use a synchronous call to the server, the needed image can not be updated in a callback method.
	 */
	public ImageDescriptor loadImage(String key);

	/**
	 * Load a list of linked elements from a given entity.
	 * 
	 * <p>
	 * This method is an asynchronous call to the server.
	 */
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener);
	
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener, int pageCount);
	
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener,String attributeList);
	
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener,String attributeList, String orderList);
	
	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener,String attributeList, String orderList, int pageCount);

	public void loadSubList(String type, int id, String linkCode, String subtype, IBeanMapListListener listener,String attributeList, int pageCount);

	/**
	 * Load a BeanMap.
	 * 
	 * <p>
	 * This method is an asynchronous call to the server.
	 * 
	 * @param type
	 *            the BeanMap type to load.
	 * @param id
	 *            the BeanMap id to load.
	 * @param listener
	 *            the callback used to return the BeanMap. This callback is not called if this beanmap does not exists.
	 */
	public void loadBeanMap(String type, int id, IBeanMapListener listener);

	/**
	 * Load a BeanMap.
	 * 
	 * <p>
	 * This method is a synchronous call to the server.
	 * 
	 * @param type
	 *            the BeanMap type to load.
	 * @param id
	 *            the BeanMap id to load.
	 * @return a BeanMap or null if an error occurs or this beanmap does-not exists.
	 */
	public BeanMap loadBeanMap(String type, int id);

	/**
	 * Load a BeanMap.
	 * 
	 * <p>
	 * This method is a synchronous call to the server.
	 * 
	 * @param type
	 *            the BeanMap type to load.
	 * @param id
	 *            the BeanMap id to load.
	 * @param attributeList
	 *            this list of the attribute we want to read
	 * @return a BeanMap or null if an error occurs or this beanmap does-not exists.
	 */
	public BeanMap loadBeanMap(String type, int id,String attributeList);
	
	
	
	/**
	 * Create the given beanMap.
	 * 
	 * <p>
	 * This operation is synchronous, it block the UI.
	 * 
	 * @param beanMap
	 * @return The new BeanMap. Or null if the operation can not complete due to connection error or conflicts.
	 */
	public BeanMap createBeanMap(BeanMap beanMap);

	/**
	 * Update the given beanMap.
	 * 
	 * <p>
	 * This operation is synchronous, it block the UI.
	 * 
	 * @param beanMap
	 * @return false if the operation can not complete due to connection error or conflicts.
	 */
	public boolean updateBeanMap(BeanMap beanMap);

	/**
	 * Create a new link between the entity defined by type/id to the entity identified as the subId according to the
	 * linkCode link element of the first entity.
	 * 
	 * <p>
	 * If the link already exist, this operation has no effect.
	 */
	public boolean putSubListItem(String type, int id, String linkCode, int subId);

	/**
	 * Delete a link between the entity defined by type/id to the entity identified as the subId according to the
	 * linkCode link element of the first entity.
	 * 
	 * <p>
	 * If the link does not exists so this operation has no effect.
	 */
	public boolean deleteSubListItem(String type, int id, String linkCode, int subId);

	/**
	 * Run a delayed loading of the specified list. Fire the changed event of this listener when the loading is
	 * completed.
	 * 
	 * @param type
	 * @param bindingListLoadRunnable
	 *            The result listener.
	 */
	public void loadList(String type, IBeanMapListListener listener);

	/**
	 * Run a delayed loading of the specified list with filter on attribute and value. Fire the changed event of this listener when the loading is
	 * completed.
	 * 
	 * @param type
	 * @param bindingListLoadRunnable
	 *            The result listener.
	 */
	public void loadList(final String type, final String attribute, final boolean equals, final Object value, final IBeanMapListListener listener);
	
	/**
	 * Get the bean stream from server to the given entity.
	 * 
	 * @param type
	 *            The entity type.
	 * @param id
	 *            The entity id.
	 * @return The input stream of the bean stream.
	 */
	public InputStream loadStream(String type, int id);

	/**
	 * Save given file on server to the given entity.
	 * 
	 * @param type
	 *            The entity type.
	 * @param id
	 *            The entity id.
	 * @param file
	 *            The file to be saved.
	 * @return true if the update is complete, false otherwise.
	 */
	public boolean updateStream(String type, int id, File file);
	
	/**
	 * Get a http redirection addresson server to the given entity.
	 * 
	 * @param type
	 *            The entity type.
	 * @param id
	 *            The entity id.
	 * @param file
	 *            The file to be saved.
	 * @return true if the update is complete, false otherwise.
	 */
	public String getUploadBeanStreamAddress(String type, int id);	
	
	public String getLastErrorMessage();
	
	/**
	 * Get latest UserMessage
	 * @return
	 */
	public UserMessage getLastErrorUserMessage();
	
	/**
	 * Load content from an URL and a given Type
	 * 
	 * @param url
	 *            URL of service
	 * @param type
	 *            The entity type.
	 * @return BeanMap result
	 */
	public BeanMap loadContent(String url, String type);

}
