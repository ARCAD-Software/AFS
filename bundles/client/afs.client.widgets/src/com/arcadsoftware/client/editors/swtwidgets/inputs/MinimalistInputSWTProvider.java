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
package com.arcadsoftware.client.editors.swtwidgets.inputs;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IEditorChangeListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.editor.swt.IWidgetValue;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;

public class MinimalistInputSWTProvider implements IInputSWTProvider, IEditorChangeListener {
	
	private MetaDataFormater formatter = null;
	private Color background;
	private String enabledIfProperty;
	private Text text;
	
	public void create(ISWTRenderer renderer, ILayoutParameters parameters, Element element, MetaDataEntity structure) {
		boolean multi = parameters.getParameterBoolean(IConstants.MULTI);
		String labelText = renderer.getLocalizedMessage(parameters.getParameter(IConstants.LABEL, element.getName()));
		if ((labelText != null) && (labelText.length() > 0)) {
			Label label = renderer.getToolkit().createLabel(renderer.getParent(), labelText);
			Label twoPoints = renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
			if(multi && (renderer.getParent().getLayout() instanceof GridLayout)) {
				label.setLayoutData(GridDataFactory.swtDefaults().align(SWT.BEGINNING, SWT.BEGINNING).create());
				twoPoints.setLayoutData(GridDataFactory.copyData((GridData) label.getLayoutData()));
			}
		}
		if (element instanceof MetaDataAttribute) {			
			int style = SWT.NONE;
			if (multi) {
				style = SWT.MULTI | SWT.V_SCROLL | SWT.WRAP;			
			}
			if (parameters.getParameterBoolean(IConstants.BORDER, true)) {
				style |= SWT.BORDER; 
			}
			text = renderer.getToolkit().createText(renderer.getParent(), "", style); //$NON-NLS-1$
			String tooltip = parameters.getParameter(IConstants.TOOLTIP);
			if ((tooltip != null) && (tooltip.length() > 0)) {
				text.setToolTipText(renderer.getLocalizedMessage(tooltip));
			}
			boolean ispassword = parameters.getParameterBoolean(IConstants.ISPASSWORD);
			if (ispassword) {
				text.setEchoChar('*');
			}
			if (parameters.getParameterBoolean(IConstants.FILL_HORIZONTAL) && (renderer.getParent().getLayout() instanceof GridLayout)) {
				GridData layoutData;
				if (multi) {
					layoutData = new GridData(GridData.FILL_BOTH);
					layoutData.grabExcessVerticalSpace = true;
				} else {
					layoutData = new GridData(GridData.FILL_HORIZONTAL);
				}
				if (labelText.length() == 0) {
					layoutData.horizontalSpan = 3;
				}
				// Traitement du tag height
				int height = parameters.getParameterInteger(IConstants.HEIGHT,-1);
				if (height > -1) {
					layoutData.grabExcessVerticalSpace = false;
					layoutData.heightHint = height;
				} else if (multi) {
					height = parameters.getParameterInteger(IConstants.LINES, 0);
					if (height > 0) {
						layoutData.grabExcessVerticalSpace = false;
						layoutData.heightHint = height * text.getLineHeight();
					}
				}
				if (parameters.getParameterBoolean(IConstants.MONOSPACE_FONT, false)) {
					text.setFont(JFaceResources.getTextFont());
				}
				text.setLayoutData(layoutData);
			}
			boolean readOnly = element.isReadonly() | parameters.getParameterBoolean(IConstants.READ_ONLY) | renderer.isReadOnly();
			text.setEditable(!readOnly);
			if (readOnly && multi) {
				text.addKeyListener(new KeyAdapter() {
						@Override
						public void keyPressed(KeyEvent e) {
							e.doit = false;
						}
					}
				);
			} else {
				text.setEnabled(!readOnly);				
			}
			if (MetaDataAttribute.TYPE_STRING.equals(element.getType()) && (((MetaDataAttribute) element).getLength() > 0)) {
				String textLimit = parameters.getParameter(IConstants.TEXTLIMIT);
				if (textLimit == null) {
					text.setTextLimit(((MetaDataAttribute) element).getLength());
				} else {
					try {
						int size = Integer.parseInt(textLimit);
						if (size != -1) {
							text.setTextLimit(size);
						}
					} catch (NumberFormatException e) {
						text.setTextLimit(((MetaDataAttribute) element).getLength());
					}
				}
			}
			if (parameters.getParameterBoolean(IConstants.DEFAULT)) {
				text.setFocus();
			}
			if (parameters.getParameterBoolean(IConstants.MANDATORY)) {
				renderer.addMandatoryAttribute(element.getCode());
			}
			if (parameters.getParameter(IConstants.MANDATORYATTRIBUTE)!=null) {
				renderer.addMandatoryAttribute(element.getCode(),parameters.getParameter(IConstants.MANDATORYATTRIBUTE));
			}
			String format = parameters.getParameter(IConstants.FORMAT);
			if (format != null && !format.isEmpty() && ((MetaDataAttribute) element).isReference()){
				formatter = new MetaDataFormater(format, structure.getEntity(((MetaDataAttribute) element).getType()));
			}
			if (formatter != null) {
				renderer.getRendererBinding().bindElement(element, bindFormattedText(renderer, element, text));
			} else {
				renderer.getRendererBinding().bindElement(element, text);
			}
			background = new Color(Display.getDefault(), 255, 255, 255);
			text.setBackground(background);
			enabledIfProperty = parameters.getParameter(IConstants.ENABLED_IF, null);
			if (enabledIfProperty != null) {
				handleEnabledIfChange(renderer);
				renderer.addChangeListener(this);
			}
		} else {
			renderer.getToolkit().createLabel(renderer.getParent(), "Not Supported yet !!!"); //$NON-NLS-1$
		}
	}
	
	private IWidgetValue bindFormattedText(final ISWTRenderer renderer, final Element element, final Text text){
		return new IWidgetValue() {			
			@Override
			public void setValue(Object newValue) {
				if (newValue instanceof BeanMap) {
					text.setText(formatter.format((BeanMap)newValue));
				}
			}
			
			@Override
			public Control getWidget() {
				return text;
			}
			
			@Override
			public Object getValueType() {
				return BeanMap.class;						
			}
			
			@Override
			public Object getValue() {
				Object obj = renderer.getCurrentBean().get(element.getCode());
				if (obj instanceof Integer) {
					return new BeanMap(element.getType(), ((Integer)obj).intValue());
				}
				if (obj instanceof BeanMap) {
					return obj;
				}
				return null;
			}

			@Override
			public void addSelectionListener(SelectionListener selectionListener) {}
		};
	}

	public void dispose() {
		background.dispose();
	}

	@Override
	public void changed(ISWTRenderer renderer) {
		handleEnabledIfChange(renderer);
	}
	
	private void handleEnabledIfChange(final ISWTRenderer renderer) {
		if ((text != null) && !text.isDisposed() && (enabledIfProperty != null)) {			
			final boolean enabled = renderer.getCurrentBean().getBoolean(enabledIfProperty);
			text.getDisplay().asyncExec(() -> this.text.setEnabled(enabled));			
		}
	}
	
	protected Text getText() {
		return text;
	}
}
