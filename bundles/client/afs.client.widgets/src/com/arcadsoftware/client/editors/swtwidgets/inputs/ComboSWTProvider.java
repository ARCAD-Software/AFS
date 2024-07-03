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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.READ_ONLY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapListEvent;
import com.arcadsoftware.beanmap.IBeanMapListListener;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.widgets.BeanMapCombo;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implement a Combo SWT Widget provider for the dynamic editors.
 */
public class ComboSWTProvider implements IInputSWTProvider {

	protected BeanMapCombo combo;
	protected ISWTRenderer renderer;

	protected String attributeFilter;
	protected String value;
	protected boolean equals;

	@Override
	public void create(ISWTRenderer swtRenderer, ILayoutParameters parameters, Element element,
			MetaDataEntity structure) {
		renderer = swtRenderer;
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		int horizontalSpan = 3;
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
			horizontalSpan = 1;
		}
		combo = createCombo(parameters, element, structure, horizontalSpan);
		final Combo widget = (Combo) combo.getWidget();
		widget.setEnabled(!element.isReadonly() && !parameters.getParameterBoolean(READ_ONLY));
		if (parameters.getParameterBoolean(DEFAULT)) {
			widget.setFocus();
		}
		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());
		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}

		/**
		 * Manager attribute Filter for list
		 */
		attributeFilter = parameters.getParameter(IConstants.ATTRIBUTEFILTER);
		if ((attributeFilter != null) && ((MetaDataAttribute) element).isReference()) {
			final MetaDataEntity attrEntity = structure.getEntity(element.getType());
			if (attrEntity != null) {
				final MetaDataAttribute attr = attrEntity.getAttribute(attributeFilter);
				if (attr != null) {
					value = parameters.getParameter(IConstants.VALUE);
					equals = parameters.getParameterBoolean(IConstants.EQUALITY, true);
				} else {
					attributeFilter = null;
				}
			} else {
				attributeFilter = null;
			}
		}

		renderer.addLoadedList(combo);
		bindElement(element);
	}

	/**
	 * Create Combo widget
	 *
	 * @param parameters
	 * @param element
	 * @param structure
	 * @param horizontalSpan
	 * @return
	 */
	protected BeanMapCombo createCombo(ILayoutParameters parameters, Element element, MetaDataEntity structure,
			int horizontalSpan) {
		return new BeanMapCombo(renderer.getParent(), SWT.READ_ONLY, parameters, renderer, element, horizontalSpan);
	}

	protected void bindElement(Element element) {

		if (attributeFilter == null) {
			renderer.getRendererBinding().bindElement(element, combo);

		} else {
			// Manually prepare list and intercept selection
			renderer.getDataLoader().loadList(element.getType(), attributeFilter, equals, value,
					new IBeanMapListListener() {
						@Override
						public void changed(final BeanMapListEvent event) {
							combo.getWidget().getDisplay().asyncExec(new Runnable() {
								@Override
								public void run() {
									combo.setBeanMapList(event.getSource());
								}
							});
						}
					});
			final Element tmp = element;
			combo.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					final BeanMap s = combo.getBeanMapValue();
					renderer.put(tmp.getCode(), (s != null) ? s.getId() : null);
				}
			});
		}
	}

	@Override
	public void dispose() {
		renderer.removeLoadedList(combo);
	}

}
