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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.ACTION;
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
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Tree;

import com.arcadsoftware.aev.core.collections.ArcadCollection;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedLabelProviderAdapter;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.framework.ui.plugins.LoggedUIPlugin;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.client.editors.swtwidgets.tools.OpenBeanMapEditor;
import com.arcadsoftware.client.editors.swtwidgets.viewer.BeanMapTreeViewer;
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

public class TreeSWTProvider
		implements IInputSWTProvider, IListenerWidget, IBeanMapSelector, ITreeSWTProvider {

	private BeanMapTreeViewer list;

	protected Action addAction;
	protected Action removeAction;
	protected Action editAction;

	private String internalEditorId;
	protected ISWTRenderer renderer;
	private ILayoutParameters parameters;
	protected MetaDataLink element;
	private ToolBarManager toolbarManager;

	@Override
	public void create(ISWTRenderer swtRenderer, ILayoutParameters layoutParameters,
			Element Element, MetaDataEntity structure) {
		renderer = swtRenderer;
		setParameters(layoutParameters);
		element = (MetaDataLink) Element;
		setInternalEditorId(getParameters().getParameter(INTERNAL_EDITOR_ID));

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

		if (getParameters().getParameterBoolean(EDIT_ACTION)) {
			editAction = createEditAction(
					renderer.getLocalizedMessage(getParameters().getParameter(EDIT_ACTION_LABEL)));
			editAction.setEnabled(!renderer.isReadOnly());
			actions.add(editAction);
		}
		if (getParameters().getParameterBoolean(REMOVE_ACTION)) {
			final Action removeAction = createRemoveAction(element, renderer.getLocalizedMessage(getParameters()
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

		final List<ElementParameter> widgetListened = getParameters().getListElementParameter(WIDGET_LISTENED);
		if (widgetListened != null) {
			for (final ElementParameter elementParameter : widgetListened) {
				renderer.addListenerWidget(this, getParameters().getElementParameter(elementParameter, WIDGET_ID));
			}
		}
	}

	protected void createControlPart(List<Action> actions) {
		createControl(renderer.getParent(), actions);
	}

	protected void createControl(Composite parent, List<Action> actions) {
		final int style = isMultiSelection() ? SWT.MULTI
				: SWT.SINGLE | SWT.FULL_SELECTION | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL;
		createControlBeforeTree(parent);
		if (getInternalEditorId() == null) {
			setList(createBeanMapTreeViewerWithoutInternalEditorId(style, actions));
		} else {
			if (editAction == null) {
				editAction = createEditAction(null);
			}
			setList(createBeanMapTreeViewerWithInternalEditorId(style, actions));
		}
		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());

		final Tree listWidget = (Tree) getList().getWidget();

		if (getParameters().getParameterBoolean(DEFAULT)) {
			listWidget.setFocus();
		}
		if (parent.getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
			layoutData.horizontalSpan = 3;
			listWidget.setLayoutData(layoutData);
			if (!getParameters().getParameterBoolean(BORDER_SPACE)) {
				final GridLayout layout = (GridLayout) parent.getLayout();
				layout.marginBottom = layout.marginHeight = layout.marginLeft = layout.marginRight = layout.marginTop = layout.marginWidth = 0;
			}
		}

		createEditorActions();
		bindElement();
		createControlAfterTree(parent);
	}

	protected void bindElement() {
		renderer.getRendererBinding().bindElement(element, getList());
	}

	public BeanMapTreeViewer getList() {
		return list;
	}

	public void setList(BeanMapTreeViewer list) {
		this.list = list;
		list.setSwtProvider(this);
		list.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if ((renderer != null) && !renderer.isReadOnly()) {
					renderer.updateFormToolbar();
				}
			}
		});
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

	private void createToolBarEditorActions() {
		final List<ElementParameter> elements = getParameters().getListElementParameter(EDITOR_ACTION);
		for (final ElementParameter elementParameter : elements) {
			if (getParameters().getElementParameterBoolean(elementParameter, IN_INTERNAL_TOOL_BAR)
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

	private Action createRemoveAction(final MetaDataLink link, final String label) {
		return new Action() {

			@Override
			public ImageDescriptor getImageDescriptor() {
				final String removeIcon = getParameters().getParameter(REMOVE_ICON);
				return (removeIcon != null) ? renderer.getImageDescriptor(removeIcon) : super.getImageDescriptor();
			}

			@Override
			public String getText() {
				return label;
			}

			@Override
			public void run() {
				final BeanMap beanMap = getList().getBeanMapValue();
				if (beanMap != null) {
					if (MessageDialog.openQuestion(LoggedUIPlugin.getShell(), renderer
							.getLocalizedMessage(getParameters().getParameter(REMOVE_CONFIRMATION_TITLE)),
							renderer
									.getLocalizedMessage(getParameters().getParameter(REMOVE_CONFIRMATION_MESSAGE)))) {
						renderer.removeLinkitem(link, beanMap);
					}
				}
			}
		};
	}

	protected BeanMapTreeViewer createBeanMapTreeViewerWithInternalEditorId(int style, List<Action> actions) {
		return new BeanMapTreeViewer(renderer.getParent(), style, renderer, getParameters(), element, actions) {
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
			public IContentProvider createContentProvider() {
				return createSpecificContentProvider();
			}

			@Override
			protected void doOnDoubleClick(IStructuredSelection selection) {
				doActionOnDoubleClick();
			}

			@Override
			protected boolean mustBeCleanValue() {
				return TreeSWTProvider.this.mustBeCleanValue();
			}

			@Override
			public String getAttributeList() {
				return TreeSWTProvider.this.getAttributeList();
			}

			@Override
			public String getOrderList() {
				return TreeSWTProvider.this.getOrderList();
			}
		};
	}

	private BeanMapTreeViewer createBeanMapTreeViewerWithoutInternalEditorId(int style, List<Action> actions) {
		return new BeanMapTreeViewer(renderer.getParent(), style, renderer, getParameters(), element, actions) {
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
				return specific == null ? super.createLabelProvider(viewer) : specific;
			}

			@Override
			public IContentProvider createContentProvider() {
				return createSpecificContentProvider();
			}

			@Override
			protected boolean mustBeCleanValue() {
				return TreeSWTProvider.this.mustBeCleanValue();
			}

			@Override
			public String getAttributeList() {
				return TreeSWTProvider.this.getAttributeList();
			}

			@Override
			public String getOrderList() {
				return TreeSWTProvider.this.getOrderList();
			}
		};
	}

	protected IContentProvider createSpecificContentProvider() {
		return null;
	}

	private void createEditorActions() {
		final List<ElementParameter> elements = getParameters().getListElementParameter(EDITOR_ACTION);

		for (final ElementParameter elementParameter : elements) {
			final String label = renderer
					.getLocalizedMessage(getParameters().getElementParameter(elementParameter, LABEL));
			final String icon = getParameters().getElementParameter(elementParameter, ICON);

			final IEditorAction action = EditorActionFactory.getEditorAction(getParameters().getElementParameter(
					elementParameter, ACTION));
			if (action != null) {
				action.setText(label);
				action.setToolTipText(label);
				if (icon != null) {
					action.setImageDescriptor(renderer.getImageDescriptor(icon));
				}
				action.setBeanMapSelector(this);
				action.setRenderer(renderer);
				action.setTreeViewer(getList());
				action.setElement(element);
				action.setInternalEditorId(getInternalEditorId());
				if (getParameters().getElementParameterBoolean(elementParameter, IN_TOOL_BAR)) {
					renderer.getRendererActions().addToolBarAction(action);
				}

				final String menuLabel = renderer
						.getLocalizedMessage(getParameters().getElementParameter(elementParameter,
								MENU_LABEL));
				if (menuLabel != null) {
					renderer.getRendererActions().addMenuAction(menuLabel, action);
				}
				if (getParameters().getElementParameterBoolean(elementParameter, IN_FORM_TOOL_BAR)) {
					renderer.addActionOnFormToolBar(action);
				}
				if (getParameters().getElementParameterBoolean(elementParameter, IN_INTERNAL_TOOL_BAR)
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

	protected boolean mustBeCleanValue() {
		return false;
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
	public void selectBeanMap(BeanMap bm) {
		getList().setBeanMapValue(bm);
	}

	@Override
	public ImageDescriptor getImageDescriptor(String key) {
		return renderer.getImageDescriptor(key);
	}

	@Override
	public BeanMapList getInput() {
		if (getList().getBeanMapList() == null) {
			return new BeanMapList();
		}
		return getList().getBeanMapList();
	}

	@Override
	public MetaDataLink getLink() {
		return element;
	}

	@Override
	public ISWTRenderer getRenderer() {
		return renderer;
	}

	@Override
	public BeanMap getSelection() {
		return getList().getBeanMapValue();
	}

	@Override
	public BeanMapTreeViewer getViewer() {
		return getList();
	}

	@Override
	public void refresh() {
		getList().refresh();
	}

	@Override
	public void setInput(BeanMapList beanMapList) {
		getList().setBeanMapList(beanMapList);
	}

	public String getInternalEditorId() {
		return internalEditorId;
	}

	public void setInternalEditorId(String internalEditorId) {
		this.internalEditorId = internalEditorId;
	}

	public ILayoutParameters getParameters() {
		return parameters;
	}

	public void setParameters(ILayoutParameters parameters) {
		this.parameters = parameters;
	}

	@Override
	public ArcadCollection createContentData(BeanMapList list) {
		return null;
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		return null;
	}

	protected void manageActions(ILayoutParameters layoutParameters) {

	}

	protected void createControlBeforeTree(Composite parent) {
	}

	protected void createControlAfterTree(Composite parent) {
	}

	public ILayoutParameters getLayoutParameters() {
		return parameters;
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

	protected void updateBeanMap() {
		final BeanMap beanMap = getList().getBeanMapValue();
		if (beanMap != null) {
			if (getInternalEditorId() == null) {
				if (canEdit(beanMap)) {
					if (editBeanMap(beanMap)) {
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
				renderer.removeLinkitem(link, beanMap);
			}
		}
	}

	protected boolean doBeforeRemovingLink(BeanMap beanMap) {
		return true;
	}

	protected boolean doAfterRemovingLink() {
		return true;
	}

	private final boolean enableMultiselection = false;

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

	protected String getAttributeList() {
		return null;
	}

	protected String getOrderList() {
		return null;
	}

	protected boolean isMultiSelection() {
		return false;
	}
}