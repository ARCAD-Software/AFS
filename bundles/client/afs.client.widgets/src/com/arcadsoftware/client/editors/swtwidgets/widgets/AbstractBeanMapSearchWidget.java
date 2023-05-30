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
package com.arcadsoftware.client.editors.swtwidgets.widgets;

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.DEFAULT;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.FORMAT;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapContainerValue;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataFormater;

public abstract class AbstractBeanMapSearchWidget implements IBeanMapContainerValue {

	private static final String BUTTON_LABEL = "..."; //$NON-NLS-1$

	private MetaDataFormater attributesFormater;
	protected Text text;
	protected Button searchButton;
	protected BeanMap beanMap;
	private ISWTRenderer renderer;
	
	public AbstractBeanMapSearchWidget(ISWTRenderer renderer, ILayoutParameters parameters, final Element element,
			int horizontalSpan) {
		super();
		this.setRenderer(renderer);
		String format = parameters.getParameter(FORMAT);
		attributesFormater = (format != null) ? new MetaDataFormater(format, renderer.getStructure(element)) : null;
		Composite composite = createContainerComposite(horizontalSpan);
		text = renderer.getToolkit().createText(composite, EMPTY, SWT.BORDER);
		text.setEnabled(false);
		text.setEditable(false);
		if (parameters.getParameterBoolean(DEFAULT)) {
			text.setFocus();
		}
		if (renderer.getParent().getLayout() instanceof GridLayout) {
			text.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}
		createContentBeforeSearchButton(composite);
		createSearchButton(composite, element);
		createContentAfterSearchButton(composite);
	}
	
	private Composite createDefaultComposite(ISWTRenderer newRenderer, int cols, int horizontalSpan) {
		Composite composite = newRenderer.getToolkit().createComposite(newRenderer.getParent(), SWT.NONE);
		GridLayout gridLayout = new GridLayout(cols, false);
		gridLayout.marginBottom = gridLayout.marginHeight = gridLayout.marginLeft = gridLayout.marginRight = gridLayout.marginTop = gridLayout.marginWidth = 0;
		composite.setLayout(gridLayout);
		if (newRenderer.getParent().getLayout() instanceof GridLayout) {
			composite.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		}
		return composite;
	}	
	
	
	/**
	 * This method create an ellipsis button that will trigger the search.
	 * The search is delegated to an object that implements ISearchBeanMap interface
	 * If the element is readonly the button will be disabled.
	 * @param composite the parent composite
	 * @param element the element on which we are working
	 */
	public void createSearchButton(Composite composite, final Element element) {
		searchButton = getRenderer().getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
		if (element.isReadonly()) {
			searchButton.setEnabled(false);
		}

		searchButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				BeanMap newBeanMap = getSelector().search(AbstractBeanMapSearchWidget.this.getRenderer().getStructure(element));
				
				if (newBeanMap != null) {
					setBeanMapValue(newBeanMap);
				} else {
					if (allowsNullValue()){
						setBeanMapValue(newBeanMap);
					}
				}
			}
		});
	}
	
	public void setBeanMapValue(BeanMap beanMap) {
		this.beanMap = beanMap;
		if (beanMap != null)
			text.setText((attributesFormater != null) ? attributesFormater.format(beanMap) : setDefaultText(beanMap));
		else
			text.setText(EMPTY);
		text.notifyListeners(SWT.Selection, new Event());
	}

	public String setDefaultText(BeanMap beanmap) {
		return beanMap.toString();
	}
	
	
	public Widget getWidget() {
		return text;
	}



	public void setRenderer(ISWTRenderer renderer) {
		this.renderer = renderer;
	}

	public ISWTRenderer getRenderer() {
		return renderer;
	}

	public boolean allowsNullValue(){
		return false;
	}
	
	public Button getSearchButton() {
		return searchButton;
	}
			
	
	/**
	 * Returns the newly created parent composite
	 * @param horizontalSpan number of horizontal span columns.<br/>
	 * Typically, this method could be override to change the number of columns
	 * provided by the composite (default = 2) if you want to add some new content
	 * after the search button 
	 * @return the parent composite of this widget
	 */
	public Composite createContainerComposite(int horizontalSpan) {
		Composite composite = createDefaultComposite(renderer, 2, horizontalSpan);
		return composite;
	}
	
	/**
	 * Use this method to add a listener to the texte
	 */
	public void addSelectionListener(SelectionAdapter selectionAdapter) {
		text.addSelectionListener(selectionAdapter);
	}
	
	/**
	 * Returns the selected beanmap
	 */
	public BeanMap getBeanMapValue() {
		return beanMap;
	}
	/**
	 * Override this method if you want to add some other content to the parent composite
	 * AFTER the seerach Button.<br/>
	 * Please note that the parent composite creation surely needs to be modified
	 * to do that, override the <code>createContainerComposite</code> method
	 * @param parent
	 * @see #createContainerComposite(int horizontalSpan)
	 */
	public void createContentAfterSearchButton(Composite parent) {
	}
	/**
	 * Override this method if you want to add some other content to the parent composite
	 * BEFORE the search button.<br/>
	 * Please note that the parent composite creation surely needs to be modified
	 * to do that, override the <code>createContainerComposite</code> method
	 * @param parent
	 * @see #createContainerComposite(int horizontalSpan)
	 */
	public void createContentBeforeSearchButton(Composite parent) {
	}	
	public abstract ISearchBeanMap getSelector();
	

}
