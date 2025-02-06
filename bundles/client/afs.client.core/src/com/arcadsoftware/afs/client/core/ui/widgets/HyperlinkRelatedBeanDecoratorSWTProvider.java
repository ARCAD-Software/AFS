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
package com.arcadsoftware.afs.client.core.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.Hyperlink;

import com.arcadsoftware.afs.client.core.connection.DataAccessHelper;
import com.arcadsoftware.afs.client.core.connection.ServerConnection;
import com.arcadsoftware.afs.client.core.ui.editors.ConnectedDynamicEditor;
import com.arcadsoftware.afs.client.core.ui.loaders.CoreContentLoader;
import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapFormater;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTDataLoader;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class HyperlinkRelatedBeanDecoratorSWTProvider implements IDecoratorSWTProvider {

	private final static String REFERENCE = "reference";
	private final static String FORMAT = "format";

	private Hyperlink hyperlink;

	private ServerConnection connection;

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {

		final String attribute = parameters.getParameter(REFERENCE, ""); //$NON-NLS-1$
		final String formatString = parameters.getParameter(FORMAT, ""); //$NON-NLS-1$

		final MetaDataAttribute relatedAttribute = structure.getAttribute(attribute);
		final String defaultLabel = relatedAttribute.getName();

		final String label = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LABEL, defaultLabel));
		if (label != null) {
			if (label.length() > 0) {
				renderer.getToolkit().createLabel(renderer.getParent(), label);
				renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
			}
		}

		connection = retrieveConnection(renderer);
		final BeanMap bean = retrieveRelatedBeanMap(renderer, structure, relatedAttribute);
		if (bean == null) {
			System.out.println("**ERROR** into HyperlinkRelatedBeanDecoratorSWTProvider");
		}
		hyperlink = renderer.getToolkit().createHyperlink(renderer.getParent(),
				"", SWT.NONE);
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			hyperlink.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, false, false, 1, 1));
		}
		if (bean != null) {
			final BeanMapFormater formatter = new BeanMapFormater(formatString, bean.getType(), false);
			final String text = formatter.format(bean);
			hyperlink.setText(text);
		}
		hyperlink.addHyperlinkListener(
				new HyperlinkAdapter() {
					@Override
					public void linkActivated(HyperlinkEvent e) {
						ConnectedDynamicEditor.openConnectedEditor(connection, bean);
					}
				});

		return hyperlink;
	}

	private ServerConnection retrieveConnection(ISWTRenderer renderer) {
		final ISWTDataLoader loader = renderer.getDataLoader();
		if (loader instanceof CoreContentLoader) {
			return ((CoreContentLoader) loader).getConnection();
		}
		return null;
	}

	private BeanMap retrieveRelatedBeanMap(ISWTRenderer renderer,
			MetaDataEntity structure, MetaDataAttribute attribute) {

		final String relatedType = attribute.getType();
		final String relatedCode = attribute.getCode();

		BeanMap current = renderer.getCurrentBean();
		while (current.getId() == 0) {
			// FIXME BUG: Si le Beanmap ne peut pas tre charg� alors on FREEZE l'application.
			try {
				Thread.sleep(50);
			} catch (final InterruptedException e) {
			}
			current = renderer.getCurrentBean();
		}
		final CoreContentLoader loader = (CoreContentLoader) renderer.getDataLoader();
		final DataAccessHelper helper = loader.getHelper(connection);

		// System.out.println(relatedType);
		// System.out.println(relatedCode);
		// System.out.println(helper==null?"HELPER NULL":"OK");
		// System.out.println(current==null?"CURRENT NULL":"id : "+current.getId());
		// if (current!=null) {
		// System.out.println("METRIC ID : "+current.getInt(relatedCode));
		// }

		final BeanMap bean = helper.read(relatedType, current.getInt(relatedCode));

		return bean;
	}

	@Override
	public void dispose() {
		if ((hyperlink != null) && !hyperlink.isDisposed()) {
			hyperlink.dispose();
		}
	}

}
