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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * This class implements an Label + Text + Search Button input provider.<br>
 * It must be subclassed to define a selector that provides the necessary GUI to let the user selecting the BeanMap<br/>
 * This kind of input is generally used when the selected beanMap is a business object and not an emuneration.<br/>
 * The property are :
 * <ul>
 * <li>label : used to define the label</li>
 * <li>mandatory : to indicate that the value is mandatory</li>
 * </ul>
 *
 * @author ARCAD Software
 * @see #getSelector()
 */
public abstract class AbstractSearchSWTProvider implements IInputSWTProvider {

	protected ISWTRenderer renderer;
	protected ILayoutParameters parameters;
	protected AbstractBeanMapSearchWidget text;
	protected Element element;

	@Override
	public void create(final ISWTRenderer renderer, ILayoutParameters parameters,
			final Element element, MetaDataEntity structure) {
		this.renderer = renderer;
		this.parameters = parameters;
		this.element = element;
		final String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}
		final int horizontalSpan = (label.length() > 0) ? 1 : 3;
		text = createBeanMapSearchWidget(renderer, parameters, element, horizontalSpan);
		// TODO RAP
		// renderer.getToolkit().paintBordersFor(renderer.getParent());
		if (parameters.getParameterBoolean(MANDATORY)) {
			renderer.addMandatoryAttribute(element.getCode());
		}
		if (textResult()) {
			final Text textWidget = (Text) text.getWidget();
			textWidget.setEditable(true);
			textWidget.setEnabled(true);
			renderer.getRendererBinding().bindElement(element, textWidget);
		} else {
			renderer.getRendererBinding().bindElement(element, text);
		}
	}

	protected boolean textResult() {
		return false;
	}

	protected AbstractBeanMapSearchWidget createBeanMapSearchWidget(ISWTRenderer renderer, ILayoutParameters parameters,
			Element element, int horizontalSpan) {
		return new AbstractBeanMapSearchWidget(renderer, parameters, element, horizontalSpan) {
			@Override
			public ISearchBeanMap getSelector() {
				return AbstractSearchSWTProvider.this.getSelector();
			}

			@Override
			public boolean allowsNullValue() {
				return AbstractSearchSWTProvider.this.allowsNullValue();
			}

			@Override
			public BeanMap getBeanMapValue() {
				BeanMap returned = super.getBeanMapValue();
				// Let the capability to make an action on the selected beanMap
				AbstractSearchSWTProvider.this.doOnSelect(returned);
				if (returned == null) {
					returned = convertSelected(returned);
					if (returned != null) {
						setBeanMapValue(returned);
					}
				}
				return returned;
			}

			@Override
			public String setDefaultText(BeanMap beanmap) {
				final String defaultText = AbstractSearchSWTProvider.this.setDefaultText(beanmap);
				if (defaultText == null) {
					return super.setDefaultText(beanmap);
				} else {
					return defaultText;
				}
			}
		};
	}

	/**
	 * Allow to define a display strategy of the beanmap when non formatter has been defined
	 *
	 * @param beanmap
	 * @return
	 */
	protected String setDefaultText(BeanMap beanmap) {
		return null;
	}

	protected boolean allowsNullValue() {
		return false;
	}

	protected void doOnSelect(BeanMap selected) {

	}

	protected BeanMap convertSelected(BeanMap selected) {
		return selected;
	}

	/**
	 * Returns a BeanMap selector
	 *
	 * @return an object that implements {@link ISearchBeanMap}
	 */
	public abstract ISearchBeanMap getSelector();

	@Override
	public void dispose() {
		// Do nothing
	}

}
