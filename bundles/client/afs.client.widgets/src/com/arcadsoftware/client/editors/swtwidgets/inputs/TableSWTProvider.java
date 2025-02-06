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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ADD_ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ADD_ACTION_LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ADD_EDITOR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ADD_ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.BORDER_SPACE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EDITOR_ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EDIT_ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EDIT_ACTION_LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EDIT_ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.INTERNAL_EDITOR_ID;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.IN_FORM_TOOL_BAR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.IN_INTERNAL_TOOL_BAR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.IN_TOOL_BAR;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MENU_LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.REMOVE_ACTION;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.REMOVE_ACTION_LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.REMOVE_CONFIRMATION_MESSAGE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.REMOVE_CONFIRMATION_TITLE;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.REMOVE_ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WIDGET_ID;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.WIDGET_LISTENED;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.ToolBar;

import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedLabelProviderAdapter;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.tools.OpenBeanMapEditor;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTableViewer;
import com.arcadsoftware.editor.ElementParameter;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapSelector;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.actions.EditorActionFactory;
import com.arcadsoftware.editor.swt.actions.IEditorAction;
import com.arcadsoftware.editor.swt.listener.IListenerWidget;
import com.arcadsoftware.editor.swt.renderer.ILoaderCallback;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataLink;

/**
 * This class implement a Table SWT Widget provider for the dynamic editors.
 */
public class TableSWTProvider implements IInputSWTProvider, IListenerWidget, IBeanMapSelector, ITableSWTProvider {

	private BeanMapTableViewer list;
	private String internalEditorId;
	private ILayoutParameters parameters;
	private ToolBarManager toolbarManager;
	private boolean storeViewerState = true;
	private String attributeList;
	private String orderList;
	private boolean enableMultiselection;
	private String subEditionAttribute;
	private String subEditionType;
	protected MetaDataLink element;
	protected Action editAction;
	protected Action addAction;
	protected Action removeAction;
	protected ISWTRenderer renderer;

	@Override
	public void create(ISWTRenderer swtRenderer, ILayoutParameters layoutParameters, Element Element,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		setParameters(layoutParameters);
		element = (MetaDataLink) Element;
		setInternalEditorId(getLayoutParameters().getParameter(INTERNAL_EDITOR_ID));
		attributeList = getLayoutParameters().getParameter(IConstants.ATTRIBUTE_LIST);
		orderList = getLayoutParameters().getParameter(IConstants.ORDER_LIST);
		subEditionAttribute = parameters.getParameter(IConstants.EDITION_ATTRIBUTE);
		subEditionType = parameters.getParameter(IConstants.EDITION_TYPE);
		// This method is called to let descendant classes having their own action manager
		manageActions(layoutParameters);
		createToolBarEditorActions();
		final List<Action> actions = new ArrayList<>();
		final List<Action> previousActions = getPreviousActions();
		if (previousActions != null) {
			for (final Action action : previousActions) {
				actions.add(action);
			}
		}
		if (getLayoutParameters().getParameterBoolean(ADD_ACTION)) {
			addAction = createAddAction(getLayoutParameters().getParameterBoolean(ADD_EDITOR), element, renderer
					.getLocalizedMessage(getLayoutParameters().getParameter(ADD_ACTION_LABEL)));
			addAction.setEnabled(!renderer.isReadOnly());
			actions.add(addAction);
		}
		if (getLayoutParameters().getParameterBoolean(EDIT_ACTION)) {
			editAction = createEditAction(
					renderer.getLocalizedMessage(getLayoutParameters().getParameter(EDIT_ACTION_LABEL)));
			editAction.setEnabled(!renderer.isReadOnly());
			actions.add(editAction);
		}
		if (getLayoutParameters().getParameterBoolean(REMOVE_ACTION)) {
			removeAction = createRemoveAction(element, renderer.getLocalizedMessage(getLayoutParameters()
					.getParameter(REMOVE_ACTION_LABEL)));
			removeAction.setEnabled(!renderer.isReadOnly());
			actions.add(removeAction);
		}
		final List<Action> nextActions = getNextActions();
		if (nextActions != null) {
			for (final Action action : nextActions) {
				actions.add(action);
			}
		}
		createControlPart(actions);
		final List<ElementParameter> widgetListened = getLayoutParameters().getListElementParameter(WIDGET_LISTENED);
		if (widgetListened != null) {
			for (final ElementParameter elementParameter : widgetListened) {
				renderer.addListenerWidget(this, getLayoutParameters().getElementParameter(elementParameter, WIDGET_ID));
			}
		}
	}

	protected void manageActions(ILayoutParameters layoutParameters) {}

	protected void createControlPart(List<Action> actions) {
		createControl(renderer.getParent(), actions);
	}

	protected void createControl(Composite parent, List<Action> actions) {
		final boolean multiSelection = getLayoutParameters().getParameterBoolean(IConstants.MULTI);
		int style = SWT.SINGLE;
		if (multiSelection) {
			style = SWT.MULTI;
			enableMultiselection = true;
		}
		style = style | SWT.FULL_SELECTION | SWT.V_SCROLL | SWT.H_SCROLL;
		if (getLayoutParameters().getParameter(IConstants.STORE_VIEWER_STATE) != null) {
			storeViewerState = getLayoutParameters().getParameterBoolean(IConstants.STORE_VIEWER_STATE);
		} else {
			storeViewerState = true;
		}
		if ((getLayoutParameters().getParameter(IConstants.BORDER) == null) || //
				getLayoutParameters().getParameterBoolean(IConstants.BORDER)) {
			style = style | SWT.BORDER;
		}
		createControlBeforeTable(renderer.getParent());
		if (getInternalEditorId() == null) {
			setList(createBeanMapTableViewerWithoutInternalEditorId(parent, style, actions));
		} else {
			if (editAction == null) {
				editAction = createEditAction(null);
			}
			setList(createBeanMapTableViewerWithInternalEditorId(parent, style, actions));
		}
		// TODO RAP  renderer.getToolkit().paintBordersFor(renderer.getParent());
		final Table listWidget = (Table) getList().getWidget();
		if (getLayoutParameters().getParameterBoolean(DEFAULT)) {
			listWidget.setFocus();
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
			final int hspan = getLayoutParameters().getParameterInteger(IConstants.COLSPAN, 1);
			layoutData.horizontalSpan = hspan;
			final int hHint = getLayoutParameters().getParameterInteger(IConstants.HEIGHT, 0);
			if (hHint > 0) {
				layoutData.heightHint = hHint;
			}
			listWidget.setLayoutData(layoutData);
			// TODO Move this parameter to the container and form objects:
			if (!getLayoutParameters().getParameterBoolean(BORDER_SPACE)) {
				final GridLayout layout = (GridLayout) renderer.getParent().getLayout();
				layout.marginBottom = 0;
				layout.marginHeight = 0;
				layout.marginLeft = 0;
				layout.marginRight = 0;
				layout.marginTop = 0;
				layout.marginWidth = 0;
			}
		}
		renderer.getRendererBinding().bindElement(element, getList(), parameters);
		createEditorActions();
		createControlAfterTable(renderer.getParent());
	}

	/**
	 * Bind Element to widget
	 */
	protected void bindElement() {
		renderer.getRendererBinding().bindElement(element, getList());
	}

	protected void createControlBeforeTable(Composite parent) {}

	protected void createControlAfterTable(Composite parent) {}

	protected boolean mustBeCleanValue() {
		return false;
	}

	protected BeanMapTableViewer createBeanMapTableViewerWithoutInternalEditorId(Composite parent, int style,
			List<Action> actions) {
		return new BeanMapTableViewer(parent, style, renderer, getLayoutParameters(), element, actions) {
			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				if (editAction != null) {
					editAction.run();
				} else {
					doActionOnDoubleClick();
				}
			}

			@Override
			public AbstractColumnedLabelProviderAdapter createLabelProvider(AbstractColumnedViewer viewer) {
				final AbstractColumnedLabelProviderAdapter specific = createSpecificLabelProvider(viewer);
				if (specific == null) {
					return super.createLabelProvider(viewer);
				}
				return specific;
			}

			@Override
			protected boolean mustBeCleanValue() {
				return TableSWTProvider.this.mustBeCleanValue();
			}

			@Override
			public String getIdentifier() {
				if (storeViewerState) {
					return super.getIdentifier();
				}
				return null;
			}

			@Override
			public String getAttributeList() {
				final String result = TableSWTProvider.this.getAttributeList();
				if (result == null) {
					return super.getAttributeList();
				}
				return result;
			}

			@Override
			public String getOrderList() {
				final String result = TableSWTProvider.this.getOrderList();
				if (result == null) {
					return super.getOrderList();
				}
				return result;
			}

			@Override
			protected String translateValue(String value, int columnIndex) {
				return TableSWTProvider.this.translateValue(value, columnIndex);
			}

			@Override
			protected String formatAlternativeValue(BeanMap bean, String rawValue,
					String columndid) {
				return TableSWTProvider.this.formatAlternativeValue(bean, rawValue, columndid);
			}

			@Override
			protected String formatMaskedValue(BeanMap bean, String rawValue,
					String columndid) {
				return TableSWTProvider.this.formatMaskedValue(bean, rawValue, columndid);
			}

			@Override
			protected Image getUserDefinedActualImage(Object element, int actualColumnIndex) {
				final Image image = TableSWTProvider.this.getUserDefinedActualImage(element, actualColumnIndex);
				if (image == null) {
					return super.getUserDefinedActualImage(element, actualColumnIndex);
				} else {
					return image;
				}
			}

		};
	}

	public Image getUserDefinedActualImage(Object element, int actualColumnIndex) {
		return null;
	}

	protected BeanMapTableViewer createBeanMapTableViewerWithInternalEditorId(Composite parent, int style,
			List<Action> actions) {
		return new BeanMapTableViewer(parent, style, renderer, getLayoutParameters(), element, actions) {
			@Override
			protected void doOnSelectionChange(IStructuredSelection selection) {
				if (editAction != null) {
					editAction.run();
				}
			}

			@Override
			public AbstractColumnedLabelProviderAdapter createLabelProvider(AbstractColumnedViewer viewer) {
				final AbstractColumnedLabelProviderAdapter specific = createSpecificLabelProvider(viewer);
				return specific == null ? super.createLabelProvider(viewer) : specific;
			}

			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doActionOnDoubleClick();
			}

			@Override
			protected boolean mustBeCleanValue() {
				return TableSWTProvider.this.mustBeCleanValue();
			}

			@Override
			protected String translateValue(String value, int columnIndex) {
				return TableSWTProvider.this.translateValue(value, columnIndex);
			}

			@Override
			public String getAttributeList() {
				final String result = TableSWTProvider.this.getAttributeList();
				if (result == null) {
					return super.getAttributeList();
				}
				return result;
			}

			@Override
			public String getOrderList() {
				final String result = TableSWTProvider.this.getOrderList();
				if (result == null) {
					return super.getOrderList();
				}
				return result;
			}

			@Override
			public void loadedListComplete(ISWTRenderer renderer) {
				super.loadedListComplete(renderer);
				TableSWTProvider.this.afterloadedListComplete();
			}

			@Override
			protected Image getUserDefinedActualImage(Object element, int actualColumnIndex) {
				final Image image = TableSWTProvider.this.getUserDefinedActualImage(element, actualColumnIndex);
				if (image == null) {
					return super.getUserDefinedActualImage(element, actualColumnIndex);
				} else {
					return image;
				}
			}

		};
	}

	public String translateValue(String value, int columnIndex) {
		return value;
	}

	private void createToolBarEditorActions() {
		final List<ElementParameter> elements = getLayoutParameters().getListElementParameter(EDITOR_ACTION);
		for (final ElementParameter elementParameter : elements) {
			if (getLayoutParameters().getElementParameterBoolean(elementParameter, IN_INTERNAL_TOOL_BAR)
					&& (toolbarManager == null)) {
				final ToolBar toolBar = new ToolBar(renderer.getParent(), SWT.NONE);
				if (renderer.getParent().getLayout() instanceof GridLayout) {
					final GridData layoutData = new GridData(GridData.HORIZONTAL_ALIGN_END);
					layoutData.horizontalSpan = 3;
					toolBar.setLayoutData(layoutData);
				}
				renderer.getToolkit().adapt(toolBar);
				toolbarManager = new ToolBarManager(toolBar);
				break;
			}
		}
	}

	private void createEditorActions() {
		final List<ElementParameter> elements = getLayoutParameters().getListElementParameter(EDITOR_ACTION);
		for (final ElementParameter elementParameter : elements) {
			final String label = renderer
					.getLocalizedMessage(getLayoutParameters().getElementParameter(elementParameter, LABEL));
			final String icon = getLayoutParameters().getElementParameter(elementParameter, ICON);
			final IEditorAction action = EditorActionFactory.getEditorAction(getLayoutParameters().getElementParameter(
					elementParameter, ACTION));
			if (action != null) {
				action.setText(label);
				action.setToolTipText(label);
				if (icon != null) {
					action.setImageDescriptor(renderer.getImageDescriptor(icon));
				}
				action.setBeanMapSelector(this);
				action.setRenderer(renderer);
				action.setTableViewer(getList());
				action.setElement(element);
				action.setInternalEditorId(getInternalEditorId());
				if (getLayoutParameters().getElementParameterBoolean(elementParameter, IN_TOOL_BAR)) {
					renderer.getRendererActions().addToolBarAction(action);
				}
				final String menuLabel = renderer.getLocalizedMessage(getLayoutParameters().getElementParameter(elementParameter, MENU_LABEL));
				if (menuLabel != null) {
					renderer.getRendererActions().addMenuAction(menuLabel, action);
				}
				if (getLayoutParameters().getElementParameterBoolean(elementParameter, IN_FORM_TOOL_BAR)) {
					renderer.addActionOnFormToolBar(action);
				}
				if (getLayoutParameters().getElementParameterBoolean(elementParameter, IN_INTERNAL_TOOL_BAR)
						&& (toolbarManager != null)) {
					toolbarManager.add(action);
					toolbarManager.update(true);
				}
			}
		}
	}

	/**
	 * Management of double click in context of internal editor
	 */
	protected void doActionOnDoubleClick() {
		// Do nothing
	}

	/**
	 * In order to change the default LabelProvider by a specific LabelProvider
	 *
	 * @param viewer
	 * @return
	 */
	protected AbstractColumnedLabelProviderAdapter createSpecificLabelProvider(AbstractColumnedViewer viewer) {
		return null;
	}

	protected List<Action> getPreviousActions() {
		return null;
	}

	protected List<Action> getNextActions() {
		return null;
	}

	protected void createBeanMap(final MetaDataLink link, final boolean withOpenEditor) {
		BeanMap beanMap = renderer.createBeanMap(new BeanMap(link.getType(), 0));
		if (beanMap != null) {
			beanMap = renderer.loadBeanMap(link.getType(), beanMap.getId());
			renderer.addLinkitem(link, beanMap);
			getList().setBeanMapValue(beanMap);
			if (withOpenEditor) {
				if (canEdit(beanMap)) {
					if (getInternalEditorId() == null) {
						if (editBeanMap(beanMap)) {
							getList().refresh();
						} else {
							renderer.getInternalEditors().loadInternalEditor(getInternalEditorId(), beanMap.getId());
							renderer.getInternalEditors().setAdd(getInternalEditorId());
						}
					}
				}
			}
		}
	}

	/**
	 * Edit Bean Map(s).
	 */
	protected void updateBeanMap() {
		if (enableMultiselection) {
			final BeanMapList beanMapList = getList().getSelected();
			if (beanMapList != null) {
				for (final BeanMap beanMap : beanMapList) {
					internalEditBeanMap(beanMap);
				}
			}
		} else {
			final BeanMap beanMap = getList().getBeanMapValue();
			internalEditBeanMap(beanMap);
		}
	}

	private void internalEditBeanMap(BeanMap beanMap) {
		if (beanMap != null) {
			BeanMap edited;
			if ((subEditionAttribute != null) && (subEditionAttribute.length() > 0)
					&& (subEditionType != null) && (subEditionType.length() > 0)) {
				edited = new BeanMap(subEditionType, beanMap.getInt(subEditionAttribute));
			} else {
				edited = beanMap;
			}
			if (getInternalEditorId() == null) {
				if (canEdit(edited)) {
					if (editBeanMap(edited)) {
						if (edited != beanMap) {
							beanMap.addAll(subEditionAttribute, edited);
						}
						getList().refresh();
					}
				}
			} else {
				renderer.getInternalEditors().loadInternalEditor(getInternalEditorId(), beanMap.getId(),
						getEditCallback());
			}
		} else if (getInternalEditorId() != null) {
			renderer.getInternalEditors().loadInternalEditor(getInternalEditorId(),
					ISWTRenderer.EMPTY_ENTITY_ID, getEditCallback());
		}
	}

	private void internalRemoveBeanMap(final MetaDataLink link, BeanMap beanMap, boolean forceConfirmation) {
		if (beanMap != null) {
			boolean continueDeletion = true;
			if (forceConfirmation) {
				continueDeletion = MessageDialog.openQuestion(LoggedUIPlugin.getShell(), renderer
						.getLocalizedMessage(getLayoutParameters().getParameter(REMOVE_CONFIRMATION_TITLE)),
						renderer
								.getLocalizedMessage(getLayoutParameters().getParameter(REMOVE_CONFIRMATION_MESSAGE)));
			}
			if (continueDeletion) {
				doBeforeRemovingLink(beanMap);
				doRemovingLink(link, beanMap);
			}
		}
	}

	protected boolean doBeforeRemovingLink(BeanMap beanMap) {
		return true;
	}

	protected void doRemovingLink(MetaDataLink link, BeanMap beanMap) {
		renderer.removeLinkitem(link, beanMap);
	}

	protected boolean doAfterRemovingLink() {
		return true;
	}

	protected void removeBeanMap(final MetaDataLink link) {

		if (enableMultiselection) {
			final BeanMapList beanMapList = getList().getSelected();
			if (beanMapList != null) {
				if (MessageDialog.openQuestion(LoggedUIPlugin.getShell(), renderer
						.getLocalizedMessage(getLayoutParameters().getParameter(REMOVE_CONFIRMATION_TITLE)),
						renderer
								.getLocalizedMessage(
										getLayoutParameters().getParameter(REMOVE_CONFIRMATION_MESSAGE)))) {

					for (final BeanMap beanMap : beanMapList) {
						internalRemoveBeanMap(link, beanMap, false);
					}
					doAfterRemovingLink();
				}
			}

		} else {
			final BeanMap beanMap = getList().getBeanMapValue();
			internalRemoveBeanMap(link, beanMap, true);
			doAfterRemovingLink();
		}
	}

	protected ILoaderCallback getEditCallback() {
		return null;
	}

	protected boolean canEdit(BeanMap beanMap) {
		return true;
	}

	protected boolean editBeanMap(BeanMap beanMap) {
		OpenBeanMapEditor.openEditor(beanMap);
		return true;
	}

	protected Action createAddAction(final boolean withOpenEditor, final MetaDataLink link, final String label) {
		return new Action(label) {
			@Override
			public ImageDescriptor getImageDescriptor() {
				final String addIcon = getLayoutParameters().getParameter(ADD_ICON);
				return (addIcon != null) ? renderer.getImageDescriptor(addIcon) : super.getImageDescriptor();
			}

			@Override
			public void run() {
				createBeanMap(link, withOpenEditor);
			}
		};
	}

	protected Action createEditAction(final String label) {
		return new Action(label) {
			@Override
			public ImageDescriptor getImageDescriptor() {
				final String editIcon = getLayoutParameters().getParameter(EDIT_ICON);
				return (editIcon != null) ? renderer.getImageDescriptor(editIcon) : super.getImageDescriptor();
			}

			@Override
			public void run() {
				updateBeanMap();
			}
		};
	}

	protected Action createRemoveAction(final MetaDataLink link, final String label) {
		return new Action(label) {

			@Override
			public ImageDescriptor getImageDescriptor() {
				final String removeIcon = getLayoutParameters().getParameter(REMOVE_ICON);
				return (removeIcon != null) ? renderer.getImageDescriptor(removeIcon) : super.getImageDescriptor();
			}

			@Override
			public void run() {
				removeBeanMap(link);
			}
		};
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	@Override
	public void refreshWidget(BeanMap beanMap) {
		renderer.getRendererBinding().refreshBean(beanMap);
	}

	@Override
	public BeanMap getSelectedBeanMap() {
		return getList().getBeanMapValue();
	}

	@Override
	public void refreshSelector(BeanMap beanMap) {
		refreshWidget(beanMap);
	}

	@Override
	public ImageDescriptor getImageDescriptor(String key) {
		return renderer.getImageDescriptor(key);
	}

	@Override
	public ISWTRenderer getRenderer() {
		return renderer;
	}

	@Override
	public BeanMapList getInput() {
		return (getList().getBeanMapList() != null) ? getList().getBeanMapList() : new BeanMapList();
	}

	@Override
	public void setInput(BeanMapList beanMapList) {
		getList().setBeanMapList(beanMapList);
	}

	@Override
	public void refresh() {
		getList().refresh();
	}

	@Override
	public BeanMap getSelection() {
		return getList().getBeanMapValue();
	}

	@Override
	public void selectBeanMap(BeanMap bm) {
		getList().setBeanMapValue(bm);
	}

	@Override
	public BeanMapTableViewer getViewer() {
		return getList();
	}

	public void setList(BeanMapTableViewer list) {
		this.list = list;
		list.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ((renderer != null) && !renderer.isReadOnly()) {
					renderer.updateFormToolbar();
				}
			}
		});
	}

	public BeanMapTableViewer getList() {
		return list;
	}

	public void setInternalEditorId(String internalEditorId) {
		this.internalEditorId = internalEditorId;
	}

	public String getInternalEditorId() {
		return internalEditorId;
	}

	public void setParameters(ILayoutParameters parameters) {
		this.parameters = parameters;
	}

	@Override
	public ILayoutParameters getLayoutParameters() {
		return parameters;
	}

	@Override
	public MetaDataLink getLink() {
		return element;
	}

	public String getAttributeList() {
		if ((attributeList != null) && (attributeList.length() == 0)) {
			return null;
		}
		return attributeList;
	}

	public String getOrderList() {
		if ((orderList != null) && (orderList.length() == 0)) {
			return null;
		}
		return orderList;
	}

	protected String formatAlternativeValue(BeanMap bean, String rawValue,
			String columndid) {
		return rawValue;
	}

	protected String formatMaskedValue(BeanMap bean, String rawValue,
			String columndid) {
		return rawValue;
	}

	/**
	 * Complete list viewer after new list is loaded
	 */
	protected void afterloadedListComplete() {
	}

	public boolean isEnableMultiselection() {
		return enableMultiselection;
	}

}
