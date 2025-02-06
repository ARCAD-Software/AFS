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
package com.arcadsoftware.editor.swt;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.widgets.FormToolkit;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.IBeanMapListener;
import com.arcadsoftware.editor.IActionElement;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.editor.swt.listener.IListenedWidget;
import com.arcadsoftware.editor.swt.listener.IListenerWidget;
import com.arcadsoftware.editor.swt.renderer.IActivated;
import com.arcadsoftware.editor.swt.renderer.IInternalEditors;
import com.arcadsoftware.editor.swt.renderer.ILoadedListListener;
import com.arcadsoftware.editor.swt.renderer.ILoadingListeners;
import com.arcadsoftware.editor.swt.renderer.IRendererActions;
import com.arcadsoftware.editor.swt.renderer.IRendererBinding;
import com.arcadsoftware.editor.swt.renderer.IUpdateDateListeners;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * This interface define the interface between SWT widget providers and the rendering of theses objects.
 */
public interface ISWTRenderer extends IBeanMapSelector {

	/**
	 * The empty entity id.
	 */
	public static final int EMPTY_ENTITY_ID = -1;

	/**
	 * @return The Eclipse form Toolkit usable to build this editor.
	 */
	public FormToolkit getToolkit();

	/**
	 * @return The current composite parent for this Widget Provider.
	 */
	public Composite getParent();

	/**
	 * Return the localized message associated with the key.
	 *
	 * @param key
	 *            The message key.
	 * @return The localized message value.
	 */
	public String getLocalizedMessage(String key);

	/**
	 * Return an Image descriptor associated with this key.
	 */
	public ImageDescriptor getImageDescriptor(String key);

	/**
	 * This method is intended to be called from deployed Container. It is used to process to a global, contained widget
	 * deployment.
	 *
	 * @param parentProvider
	 *            The supposed invoking provider.
	 * @param parameters
	 *            The current list of parameter (must be the actual parameters object).
	 * @param parent
	 *            the Composite parent for the sub-widgets.
	 */
	public void createSubContainer(IContainerSWTProvider parentProvider, ILayoutParameters parameters,
			Composite parent);

	/**
	 * This method is intended to be called from deployed Container. It is used to process to a global, contained widget
	 * deployment.
	 *
	 * @param parentProvider
	 *            The supposed invoking provider.
	 * @param messageManager
	 *            The message manager associated with sub-controls and will decorate it.
	 * @param parameters
	 *            The current list of parameter (must be the actual parameters object).
	 * @param parent
	 *            the Composite parent for the sub-widgets.
	 * @param actions
	 *            Collect the sub-widgets actions.
	 */
	public void createSubContainer(IContainerSWTProvider parentProvider, IMessageManager messageManager,
			ILayoutParameters parameters, Composite parent, ArrayList<IAction> actions);

	/**
	 * Return the provider responsible for the creation of the parent composite.
	 * <p>
	 * This provider represent the direct parent inthe layout tree. It is assumed to be the creator of the parent
	 * composite but some container do not really create a Composite to place sub-widget into it.
	 *
	 * @return null for top level widgets, or the parent container provider.
	 */
	public IContainerSWTProvider getParentProvider();

	/**
	 * Add a event listener fired when the edited BeanMap is loaded.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void addLoadListener(IBeanMapChangedListener listener);

	/**
	 * Remove a event listener fired when the edited BeanMap is loaded.
	 * <p>
	 * Has no affect if an identical listener is not registered.
	 *
	 * @param listener
	 */
	public void removeLoadListener(IBeanMapChangedListener listener);

	/**
	 * Remove a event listener fired when the edited BeanMap is saved.
	 * <p>
	 * Has no affect if an identical listener is not registered.
	 *
	 * @param listener
	 */
	public void removeSaveListener(IBeanMapChangedListener listener);

	/**
	 * Add a event listener fired when the edited BeanMap is saved.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void addSaveListener(IBeanMapChangedListener listener);

	/**
	 * Add a listener called to control BeanMap Validity before saving.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void addBeforeSaveControler(IBeanMapControlerListener listener);

	/**
	 * Remove a listener called to control BeanMap Validity before saving.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void removeBeforeSaveControler(IBeanMapControlerListener listener);

	/**
	 * Add a event listener fired when the edited BeanMap is saved.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void addBeanMapSaveListener(IBeanMapSavedListener listener);

	/**
	 * Remove a event listener fired when the edited BeanMap is saved.
	 * <p>
	 * Has no affect if an identical listener is not registered.
	 *
	 * @param listener
	 */
	public void removeBeanMapSaveListener(IBeanMapSavedListener listener);

	/**
	 * Remove a event listener fired when a control test fails.
	 * <p>
	 * Has no affect if an identical listener is not registered.
	 *
	 * @param listener
	 */
	public void removeValidityListener(IControlValidityListener listener);

	/**
	 * Add a event listener fired when a control test fails.
	 * <p>
	 * Has no effect if an identical listener is already registered.
	 *
	 * @param listener
	 */
	public void addValidityListener(IControlValidityListener listener);

	/**
	 * @return true if the current BeanMap has been changed.
	 */
	public boolean isDirty();

	/**
	 * Force the editor into the dirty state (will cause the current BeanMap to be mark as changed).
	 */
	public void forceDirty();

	/**
	 * @return true if the current BeanMap is coherent and can be saved.
	 */
	public boolean isRecordable();

	/**
	 * Reload the BeanMap from the server (or at least from the cache).
	 * <p>
	 * Any modification will be lost.
	 */
	public void reload();

	/**
	 * Get an SWT color from a CSS textual representation of this color.
	 * <p>
	 * The caller is responsible for disposing this Color
	 *
	 * @param cssColor
	 *            The CSS color name.
	 * @return null if the color name is invalid.
	 */
	public Color getColor(String cssColor);

	/**
	 * Return the specified Action as an SWT Action.
	 *
	 * @param action
	 *            The action
	 */
	public IAction getAction(IActionElement action);

	/**
	 * Return the specified Action as an SWT Action.
	 *
	 * @param code
	 *            The action code.
	 */
	public IAction getAction(String code);

	/**
	 * Declare the specified action as a global editor action.
	 *
	 * @param action
	 *            The action
	 */
	public void declareGlobalAction(IActionElement action);

	/**
	 * Register an SWT action as an Action that will be associated to the direct parent able to attach action to itself.
	 * <p>
	 * This action can be a Java action or a transformed JavaScript action (IActionElement).
	 *
	 * @param action
	 *            the Action.
	 */
	public void registerAction(IAction action);

	/**
	 * Return a content provider for a tree representation based of an list of links.
	 * <p>
	 * Each element of the list is supposed to possess and attribute that return the parent item in the tree. This item
	 * must be in the current list.
	 */
	public ITreeContentProvider getTreeContentProvider(MetaDataLink link, String fatherCode);

	/**
	 * Add an item into a specified links list. This modification will automatically reported to any binded widgets.
	 * <p>
	 * This item must exist on the server !
	 * <p>
	 * <b>This means that the <code>item</code> id can not be null.</b>
	 */
	public void addLinkitem(MetaDataLink link, BeanMap item);

	/**
	 * Remove an item from a given links list.This modification will automatically reported to any binded widgets.
	 */
	public void removeLinkitem(MetaDataLink link, BeanMap item);

	/**
	 * Returns the data loader.
	 * <p>
	 * Client should not use the Data loader directly but use the bindings provided by the renderer. The renderer
	 * maintain list to update on the server.
	 * <p>
	 * If a widget provider need to access to the server in a way not corresponding to the binding of this renderer it
	 * should use a direct access to the server without dependencies from the renderer.
	 *
	 * @return the dataLoader.
	 */
	public ISWTDataLoader getDataLoader();

	/**
	 * Returns the entity referenced by the given <code>element</code>.
	 * <p>
	 * For instance if the element is a link the returned entity is the destination entity.
	 * <p>
	 * This method can return null if the referenced entity can not be loaded, or if the element is an attribute with a
	 * atomic type (string, integer, ...).
	 *
	 * @param element
	 *            An attribute or a link that referenced another entity.
	 * @return The <code>element</code>'s referenced structure.
	 */
	public MetaDataEntity getStructure(Element element);

	/**
	 * This method return the current BeanMap without binding.
	 * <p>
	 * This bean must be used ads a read-only value. You must use the data bindings to set data values.
	 * <p>
	 * From this BeanMap you can obtain id, type and last modification date informations.
	 *
	 * @return The currently edited BeanMap.
	 */
	public BeanMap getCurrentBean();

	/**
	 * Get the bean stream from server to the given entity.
	 *
	 * @param type
	 *            The entity type.
	 * @param id
	 *            The entity id.
	 * @return The input stream of the bean stream.
	 */
	public InputStream getBeanStream(String type, int id);

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
	public boolean updateBeanStream(String type, int id, File file);

	/**
	 * get a http redirection addresse on server to the given entity.
	 *
	 * @param type
	 *            The entity type.
	 * @param id
	 *            The entity id.
	 * @return true if the update is complete, false otherwise.
	 */
	public String getUploadBeanStreamAddress(String type, int id);

	/**
	 * Returns the entity beanMap from server.
	 *
	 * @param type
	 *            The entity type to be loaded.
	 * @param id
	 *            The entity id to be loaded.
	 * @return The entity beanMap loading from server.
	 */
	public BeanMap loadBeanMap(String type, int id);

	/**
	 * Returns the entity beanMap from server.
	 *
	 * @param type
	 *            The entity type to be loaded.
	 * @param id
	 *            The entity id to be loaded.
	 * @param attributeList
	 *            The attribute list to read
	 * @return The entity beanMap loading from server.
	 */
	public BeanMap loadBeanMap(String type, int id, String attributeList);

	/**
	 * Returns asynchronously the requested BeanMap.
	 *
	 * @param type
	 * @param id
	 * @param listener
	 */
	public void loadBeanMap(String type, int id, IBeanMapListener listener);

	/**
	 * Returns the title image. Used to define the editor image.
	 *
	 * @return The title image.
	 */
	public Image getTitleImage();

	/**
	 * Returns the Help Context Id. Used to define the editor Dynamic Help.
	 *
	 * @return The Help Id.
	 */
	public String getHelpContextId();

	/**
	 * Returns the entity structure for the given entity type.
	 *
	 * @param type
	 *            The entity type.
	 * @return The entity structure for the given entity type.
	 */
	public MetaDataEntity getStructure(String type);

	/**
	 * Creates a new beanMap.
	 *
	 * @param beanMap
	 *            The beanMap to be created.
	 * @return The beanMap created.
	 */
	public BeanMap createBeanMap(BeanMap beanMap);

	/**
	 * Add a change listener on internal editor.
	 *
	 * @param listener
	 *            The listener.
	 */
	public void addChangeListener(IEditorChangeListener listener);

	/**
	 * Add a change listener on internal editor.
	 *
	 * @param listener
	 *            The listener.
	 */
	public void removeChangeListener(IEditorChangeListener listener);

	/**
	 * Returns the beanMap update date.
	 *
	 * @return The beanMap update date.
	 */
	public Date getUpdateDate();

	/**
	 * Add the given widget to list of listener widget.
	 *
	 * @param listenerWidget
	 *            the listener widget.
	 * @param widgetId
	 *            the widget to be listened.
	 */
	public void addListenerWidget(IListenerWidget listenerWidget, String widgetId);

	/**
	 * Fires event when listened widget changed.
	 *
	 * @param listenedWidget
	 *            the listened widget.
	 * @param beanMap
	 *            the new beanMapValue
	 */
	public void fireListenedWidgetChanged(IListenedWidget listenedWidget, BeanMap beanMap);

	/**
	 * Returns true if editor can be saved, false otherwise.
	 *
	 * @return true if editor can be saved, false otherwise.
	 */
	public boolean canSavedEditor();

	/**
	 * Returns true if editor can be saved, false otherwise.
	 *
	 * @return true if editor can be saved, false otherwise.
	 */
	public boolean canSavedEditor(boolean withErrorMessage);

	/**
	 * Returns true if editor can be saved, false otherwise.
	 *
	 * @return true if editor can be saved, false otherwise.
	 */
	public boolean canSavedEditor(IEntityAttributeProcessListener callback);

	/**
	 * Defines the attribute as mandatory.
	 *
	 * @param code
	 *            The attribute code.
	 */
	public void addMandatoryAttribute(String code);

	/**
	 * Defines the attribute as mandatory.
	 *
	 * @param code
	 *            The attribute code.
	 * @param conditionnedBy
	 *            (attribute) The value of the beanmap given by <b>code</b> will be mandatory if the value of the
	 *            beanmap given by <b>conditionnedBy</b> is true.
	 */
	public void addMandatoryAttribute(String code, String conditionnedBy);

	/**
	 * @return the internalEditors
	 */
	public IInternalEditors getInternalEditors();

	/**
	 * @return the loadingListeners
	 */
	public ILoadingListeners getLoadingListeners();

	/**
	 * @return the updateDateListeners
	 */
	public IUpdateDateListeners getUpdateDateListeners();

	/**
	 * @return the rendererActions
	 */
	public IRendererActions getRendererActions();

	/**
	 * @return the rendererBinding
	 */
	public IRendererBinding getRendererBinding();

	/**
	 * Refresh editor content with the given beanMap.
	 *
	 * @param beanMap
	 *            the beanMap to refresh
	 * @param renderer
	 *            the renderer which have modified the given beanMap
	 */
	public void refreshEditorContent(BeanMap beanMap, ISWTRenderer renderer);

	/**
	 * Registers the form toolBar manager.
	 *
	 * @param toolBarManager
	 *            the toolBar manager.
	 */
	public void setFormToolBar(IToolBarManager toolBarManager);

	/**
	 * Add an action in form toolBar manager.
	 *
	 * @param action
	 *            the action.
	 */
	public void addActionOnFormToolBar(IEditorAction action);

	/**
	 * Record the modifications.
	 *
	 * @return true if saved, false otherwise
	 */
	public boolean save();

	/**
	 * @return true if editor is read only, false otherwise
	 */
	public boolean isReadOnly();

	/**
	 * Link the internal editor with this parentRenderer
	 *
	 * @param renderer
	 */
	public void setParentRenderer(ISWTRenderer renderer);

	/**
	 * Used by internal editors to get parent editor (and access to parent BeanMap).
	 *
	 * @return the parent renderer
	 */
	public ISWTRenderer getParentRenderer();

	/**
	 * Add a listener called when a list of linked value is ready to be used.
	 *
	 * @param listener
	 */
	public void addLoadedList(ILoadedListListener listener);

	/**
	 * Remove a listener.
	 *
	 * @param listener
	 * @see #addLoadedList(ILoadedListListener)
	 */
	public void removeLoadedList(ILoadedListListener listener);

	public void refreshAllEditors(BeanMap beanMap, ISWTRenderer renderer);

	public void putVirtualValue(String key, Object value);

	public Object put(String key, Object value);

	public Object getVirtualValue(String key);

	public List<MandatoryAttribute> getMandatoryAttributes();

	public MetaDataEntity getStructure();

	public int getId();

	public void updateFormToolbar();

	public IToolBarManager getFormToolBarManager();

	public void setParentReadOnly(boolean readOnly);

	public void addActivatedListeners(IActivated listener);

	public void removeActivatedListeners(IActivated listener);

	public void fireActivatedEvent();

	/**
	 * Get the Message manager that allow to SWT Providers to populate some specific messages into the Form.
	 * 
	 * @return
	 */
	public IMessageManager getMessageManager();

	/**
	 * Run an ISCriptAction corresponding to the specific ID.
	 * <p>
	 * The renderer set some default parameters (source entity, current edited item and so on...) the given parameters
	 * will override these parameters.
	 *
	 * @param string
	 * @param parameters
	 * @return the "result" of this action.
	 */
	public Object runScriptAction(String action, Map<String, Object> parameters);

}
