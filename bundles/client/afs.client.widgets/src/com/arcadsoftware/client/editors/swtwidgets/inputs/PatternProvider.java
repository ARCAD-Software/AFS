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

import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IMessageManager;

import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

public class PatternProvider implements IInputSWTProvider {

	@Override
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity entity) {
		if (!(element instanceof MetaDataAttribute)) {
			return;
		}
		final String label = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LABEL, element.getName()));
		if ((label != null) && (label.length() > 0)) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
		}

		final Text text = renderer.getToolkit().createText(renderer.getParent(), "", SWT.BORDER); //$NON-NLS-1$
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			final GridData layoutData = new GridData();
			if (parameters.getParameterBoolean(IConstants.FILL_HORIZONTAL)) {
				layoutData.horizontalAlignment = GridData.FILL;
			}
			if (label.length() == 0) {
				layoutData.horizontalSpan = 3;
			}
			// <FM number="2010/564" version="09.03.02" date=Dec 7, 2010 user=md>
			// Traitement du tag height
			final int height = parameters.getParameterInteger(IConstants.HEIGHT, -1);
			if (height > -1) {
				layoutData.heightHint = height;
			}
			// </FM>
			text.setLayoutData(layoutData);
		}
		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());
		if (element.isReadonly()) {
			text.setEditable(false);
		}
		// if (element.isStringType() && (((MetaDataAttribute) element).getLength() > 0)) {
		if (MetaDataAttribute.TYPE_STRING.equals(element.getType())
				&& (((MetaDataAttribute) element).getLength() > 0)) {
			text.setTextLimit(((MetaDataAttribute) element).getLength());
		}
		if (parameters.getParameterBoolean(IConstants.DEFAULT)) {
			text.setFocus();
		}
		if (parameters.getParameterBoolean(IConstants.MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		final String p = parameters.getParameter("pattern"); //$NON-NLS-1$
		if ((p != null) && (p.trim().length() > 0)) {
			final IMessageManager messages = renderer.getMessageManager();
			final Pattern pattern = Pattern.compile(p);
			final String messagekey = "pattern.warn." + element.getCode(); //$NON-NLS-1$
			final String message = renderer.getLocalizedMessage(
					parameters.getParameter("message", String.format(Messages.patternMessage, element.getName()))); //$NON-NLS-1$
			text.addModifyListener(new ModifyListener() {
				@Override
				public void modifyText(ModifyEvent e) {
					if (!text.isDisposed()) {
						if ((text.getText() == null) || (text.getText().length() == 0)
								|| pattern.matcher(text.getText()).matches()) {
							messages.removeMessage(messagekey, text);
						} else {
							messages.addMessage(messagekey, message, null, IMessageProvider.WARNING, text);
						}
					}
				}
			});
		}
		renderer.getRendererBinding().bindElement(element, text);
	}

	@Override
	public void dispose() {
	}

}
