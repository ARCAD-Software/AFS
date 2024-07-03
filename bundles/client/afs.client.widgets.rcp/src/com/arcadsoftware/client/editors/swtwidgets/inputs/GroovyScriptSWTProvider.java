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

import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.bindings.keys.ParseException;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.SimpleContentProposalProvider;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.linestylers.GroovyLineStyler;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class GroovyScriptSWTProvider implements IInputSWTProvider, IConstants {

	private static final String COMPLETIONKEY = "completionKey"; //$NON-NLS-1$
	private static final String API = "api"; //$NON-NLS-1$

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity entity) {
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label != null) {
			if (label.length() > 0) {
				renderer.getToolkit().createLabel(renderer.getParent(), label);
				renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
			}
		}
		int style = SWT.MULTI | SWT.V_SCROLL | SWT.WRAP;
		if (element.isReadonly() || parameters.getParameterBoolean(READ_ONLY)) {
			style |= SWT.READ_ONLY;
		}
		final StyledText text = new StyledText(renderer.getParent(), style);
		renderer.getToolkit().adapt(text, true, true);
		renderer.getToolkit().paintBordersFor(renderer.getParent());
		final String[] apis = getApisMethodNames(parameters, element, entity);
		final String completionKey = parameters.getParameter(COMPLETIONKEY);
		if ((apis != null) && (apis.length > 0) && (completionKey != null)) {
			final SimpleContentProposalProvider scp = new SimpleContentProposalProvider(apis);
			scp.setProposals(apis);
			KeyStroke ks;
			try {
				ks = KeyStroke.getInstance(completionKey);
			} catch (final ParseException e) {
				ks = KeyStroke.getInstance(SWT.F10);
			}
			final ContentProposalAdapter adapter = new ContentProposalAdapter(text, new TextContentAdapter(), scp, ks,
					null);
			adapter.setProposalAcceptanceStyle(ContentProposalAdapter.PROPOSAL_REPLACE);
		}
		// TODO Ajouter une gestion contextuelle de l'aide en ligne (dans une popup...)
		text.addLineStyleListener(new GroovyLineStyler(apis));
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData(GridData.FILL_BOTH);
			layoutData.grabExcessVerticalSpace = true;
			if (label.length() == 0) {
				layoutData.horizontalSpan = 3;
			}
			layoutData.minimumHeight = 80;
			// Traitement du tag height
			final int height = parameters.getParameterInteger(HEIGHT, -1);
			if (height > -1) {
				layoutData.heightHint = height;
			}
			text.setLayoutData(layoutData);
		}
		if (MetaDataAttribute.TYPE_STRING.equals(element.getType())
				&& (((MetaDataAttribute) element).getLength() > 0)) {
			text.setTextLimit(((MetaDataAttribute) element).getLength());
		}
		if (parameters.getParameterBoolean(DEFAULT)) {
			text.setFocus();
		}
		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		renderer.getRendererBinding().bindElement(element, text);

	}

	/**
	 * Override this method to add a completion list of methods names.
	 *
	 * @param parameters
	 * @param element
	 * @param entity
	 * @return can return null.
	 */
	protected String[] getApisMethodNames(ILayoutParameters parameters, Element element, MetaDataEntity entity) {
		final String api = parameters.getParameter(API);
		if ((api == null) || (api.trim().length() == 0)) {
			return null;
		}
		return api.split(" "); //$NON-NLS-1$
	}

	@Override
	public void dispose() {}

}
