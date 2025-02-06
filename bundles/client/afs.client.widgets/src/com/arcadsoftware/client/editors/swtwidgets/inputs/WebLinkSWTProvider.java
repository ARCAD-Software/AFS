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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.HTTP;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MENU_ICON;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;
import static org.eclipse.ui.browser.IWorkbenchBrowserSupport.LOCATION_BAR;
import static org.eclipse.ui.browser.IWorkbenchBrowserSupport.NAVIGATION_BAR;
import static org.eclipse.ui.browser.IWorkbenchBrowserSupport.STATUS;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;

import com.arcadsoftware.aev.core.ui.actions.MenuAction;
import com.arcadsoftware.client.editors.swtwidgets.internal.Activator;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Web link SWT Widget provider for the dynamic editors.
 */
public class WebLinkSWTProvider implements IInputSWTProvider {

	protected Text text;
	protected URL url;
	protected MenuManager menuManager = new MenuManager();
	protected ISWTRenderer renderer;

	@Override
	public void create(ISWTRenderer swtRenderer, final ILayoutParameters parameters, final Element element,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		int horizontalSpan = 4;
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		} else {
			horizontalSpan = horizontalSpan - 2;
		}

		final Composite composite = createDefaultComposite(horizontalSpan);

		text = renderer.getToolkit().createText(composite, EMPTY, SWT.BORDER);
		text.setEnabled(!element.isReadonly());
		if (parameters.getParameterBoolean(DEFAULT)) {
			text.setFocus();
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			text.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}

		text.addModifyListener(event -> {
			if ((text.getText() != null) && (text.getText().length() > 0)) {
				if (text.getText().contains(HTTP)) {
					try {
						url = new URL(text.getText());
						text.setBackground(new Color(null, 255, 255, 255));
					} catch (final MalformedURLException e) {
						url = null;
						text.setBackground(new Color(null, 255, 0, 0));
					}
				} else {
					url = null;
					text.setBackground(new Color(null, 255, 0, 0));
				}
			} else {
				url = null;
				text.setBackground(new Color(null, 255, 255, 255));
			}
		});

		final ToolBar toolbar = new ToolBar(composite, SWT.FLAT);
		final ToolBarManager toolbarManager = new ToolBarManager(toolbar);
		final MenuAction menuAction = new MenuAction(EMPTY, IAction.AS_DROP_DOWN_MENU, menuManager);

		menuAction.setImageDescriptor(renderer.getImageDescriptor(parameters.getParameter(MENU_ICON)));
		toolbarManager.add(menuAction);
		toolbarManager.update(true);

		menuManager.add(new Action(Messages.WebLinkSWTProvider_ViewButton) {
			@Override
			public void run() {
				if (url != null) {
					try {
						final IWebBrowser browser = PlatformUI.getWorkbench().getBrowserSupport().createBrowser(
								NAVIGATION_BAR | STATUS | LOCATION_BAR, null, text.getText(), null);
						browser.openURL(url);
					} catch (final PartInitException e) {
						Activator.getInstance().log(e);
					}
				}
			}
		});

		completeMenuManager(text, parameters, element, composite);

		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		renderer.getRendererBinding().bindElement(element, text);
	}

	protected void completeMenuManager(Text swtText,
			ILayoutParameters parameters,
			Element element, Composite composite) {
		// Do nothing
	}

	@Override
	public void dispose() {
		// Do nothing
	}

	private Composite createDefaultComposite(int cols) {
		final Composite composite = renderer.getToolkit().createComposite(renderer.getParent(), SWT.NONE);
		final GridLayout gridLayout = new GridLayout(cols, false);
		gridLayout.marginBottom = gridLayout.marginHeight = gridLayout.marginLeft = gridLayout.marginRight = gridLayout.marginTop = gridLayout.marginWidth = 0;
		composite.setLayout(gridLayout);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}
		return composite;
	}
}
