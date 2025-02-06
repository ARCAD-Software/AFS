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

import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.editor.implementation.swt.renderer.SWTRenderer;

/**
 * The <code>EditorComposite</code> is multi-purpose Dynamic editor place holder.
 */
public class DynamicEditorComposite extends Composite {

	private SWTRenderer renderer;

	/**
	 * Construct the dynamic Editor according to the specific entity type and layout name given.
	 * <p>
	 * The construction load the editor layout, and the structurals informations about the edited BeanMap.
	 * <p>
	 * When it is done the beanMap must be
	 *
	 * @param parent
	 *            A widget which will be the parent of the new instance (cannot be null)
	 * @param realm
	 *            The Dynamic Editor realm (null if there is only one).
	 * @param type
	 *            The Entity type to be edited.
	 * @param layoutName
	 *            The Editor layout document name.
	 * @param readOnly
	 *            the readOnly value.
	 */
	public DynamicEditorComposite(Composite parent, int style, String realm, String type, String layoutName,
			boolean readOnly) {
		this(parent, style, realm, type, layoutName, readOnly, true);
	}

	public DynamicEditorComposite(Composite parent, int style, String realm, String type, String layoutName,
			boolean readOnly, boolean createContent) {
		super(parent, style);
		initContent(parent, realm, type, layoutName, readOnly, createContent);
	}

	/**
	 * Simple constructor; no content initialization. Must be completed by calling initContent
	 *
	 * @param parent
	 * @param style
	 */
	public DynamicEditorComposite(Composite parent, int style) {
		super(parent, style);
	}

	/**
	 * Init content, possibly out of constructor
	 *
	 * @param parent
	 * @param realm
	 * @param type
	 * @param readOnly
	 */
	protected void initContent(Composite parent, String realm, String type, String layoutName, boolean readOnly,
			boolean createContent) {
		// renderer = new SWTRenderer(realm, type, readOnly);
		renderer = new SWTRenderer(parent.getDisplay(), realm, type, readOnly) {
			@Override
			protected void dataLoaderCreated(SWTRenderer renderer) {
				initializeDataLoader(renderer);
			}

			@Override
			protected void editorLoaderCreated(SWTRenderer renderer) {
				initializeEditorLoader(renderer);
			}
		};
		setLayout(new FillLayout());
		if (createContent) {
			createPartControl(layoutName);
		}
	}

	public void createPartControl(String layoutName) {
		renderer.createPartControl(this, layoutName);
	}

	public DynamicEditorComposite(Composite parent, String realm, String type, String layoutName, boolean readOnly) {
		this(parent, 0, realm, type, layoutName, readOnly);
	}

	protected void initializeEditorLoader(SWTRenderer renderer) {
	}

	protected void initializeDataLoader(SWTRenderer renderer) {
	}

	/**
	 * Construct the dynamic Editor according to the specific entity type and layout name given.
	 * <p>
	 * The construction load the editor layout, and the structurals informations about the edited BeanMap.
	 * <p>
	 * When it is done the beanMap must be
	 *
	 * @param parent
	 *            A widget which will be the parent of the new instance (cannot be null)
	 * @param realm
	 *            The Dynamic Editor realm (null if there is only one).
	 * @param type
	 *            The Entity type to be edited.
	 * @param layoutName
	 *            The Editor layout document name.
	 */
	public DynamicEditorComposite(Composite parent, String realm, String type, String layoutName) {
		this(parent, realm, type, layoutName, false);
	}

	/**
	 * Load the corresponding BeanMap for the given type (see constructor) and identifier.
	 *
	 * @param id
	 *            A valid BeanMap identifier
	 * @return true if the loading process complete.
	 */
	public boolean load(int id) {
		return renderer.load(id);
	}

	/**
	 * Load the corresponding BeanMap for the given type (see constructor) and identifier. And notify changed when
	 * beanMap loaded
	 *
	 * @param id
	 *            A valid BeanMap identifier
	 */
	public void load(BeanMap beanMap) {
		renderer.loadBeanMap(new BeanMapEvent(beanMap));
	}

	/**
	 * Load the an empty BeanMap for the given type (see constructor).
	 *
	 * @return true if the loading process complete.
	 */
	public boolean loadEmptyEntity() {
		return load(ISWTRenderer.EMPTY_ENTITY_ID);
	}

	/**
	 * Save the currently edited BeanMap.
	 */
	public boolean save() {
		return renderer.save();
	}

	/**
	 * Get a copy of the currently edited BeanMap.
	 * <p>
	 * Any change made to this BeanMap will not reported to the editor. To do so, you must use the ISWTRenderer
	 * interface.
	 *
	 * @return The currently edited BeanMap.
	 * @see DynamicEditorComposite#getRenderer()
	 */
	public BeanMap getCurrent() {
		return renderer.getCurrentBean();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Widget#dispose()
	 */
	@Override
	public void dispose() {
		renderer.dispose();
		super.dispose();
	}

	/**
	 * Access to the renderer interface can be used for low level tuning.
	 *
	 * @return The renderer interface.
	 */
	public ISWTRenderer getRenderer() {
		return renderer;
	}

	public List<IAction> getBasicActions() {
		return renderer.getRendererActions().getToolBarActions();
	}

	public List<IAction> getGlobalActions() {
		return renderer.getGlobalActions();
	}

	/**
	 * @return The selection provider associate to this Composite.
	 */
	public ISelectionProvider getSelectionProvider() {
		return renderer;
	}

	/**
	 * Reload the editor contents from server.
	 * <p>
	 * Any modification will be lost.
	 */
	public void reload() {
		renderer.reload();
	}

	/**
	 * @return true if the current entity is an empty entity, false otherwise
	 */
	public boolean isEmptyEntity() {
		return renderer.getCurrentBean().getId() == ISWTRenderer.EMPTY_ENTITY_ID;
	}

	/**
	 * @return true if the current BeanMap has been changed.
	 */
	public boolean isDirty() {
		return renderer.isDirty();
	}

	public void setInputEnabled(boolean b) {
		setChildrenControlEnabled(b, this);
	}

	private void setChildrenControlEnabled(boolean enable, Composite composite) {
		final Control[] controls = composite.getChildren();
		for (final Control control : controls) {
			if ((control instanceof Composite) && !(control instanceof Combo)) {
				setChildrenControlEnabled(enable, (Composite) control);
			} else if (control instanceof Text) {
				((Text) control).setEditable(enable);
			} else {
				control.setEnabled(enable);
			}
		}
	}
}
