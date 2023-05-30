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

import static com.arcadsoftware.client.editors.swtwidgets.IConstants.EMPTY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.beanmap.BeanMapEvent;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IBeanMapChangedListener;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * 
 */
public abstract class AbstractDownloadSWTProvider implements IInputSWTProvider {

	protected static final String BUTTON_LABEL = "..."; //$NON-NLS-1$
	protected String filePath;

	public void create(final ISWTRenderer renderer, final ILayoutParameters parameters, Element element,
			final MetaDataEntity structure) {
		String label = renderer.getLocalizedMessage(parameters.getParameter(LABEL, element.getName()));
		if (label.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), label);
			renderer.getToolkit().createLabel(renderer.getParent(), TWO_POINTS);
		}

		int horizontalSpan = (label.length() > 0) ? 1 : 3;
		final Composite composite = createDefaultComposite(renderer, horizontalSpan);

		final Text text = renderer.getToolkit().createText(composite, EMPTY, SWT.BORDER);
		text.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		text.setEditable(false);
		text.setEnabled(!element.isReadonly());

		
//		Button browseButton = renderer.getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
//		browseButton.addSelectionListener(new SelectionAdapter() {
//			@Override
//			public void widgetSelected(SelectionEvent e) {
//				FileInfo fileInfo = getFilename(composite);
//				if (fileInfo!=null){
//					filePath = fileInfo.getAbsoluteFilename();
//					text.setText(fileInfo.getRelativeFilename());					
//				}				
//			}
//		});
//
//		browseButton.setEnabled(!element.isReadOnly());
//		Button openButton = renderer.getToolkit().createButton(composite, Messages.DownloadSWTProvider_OpenFileButton,
//				SWT.PUSH);
//		openButton.addSelectionListener(new SelectionAdapter() {
//			@Override
//			public void widgetSelected(SelectionEvent e) {
//				if (filePath != null) {
//					OpenFile.openFile(new File(filePath));
//				} else {
//					OpenFile.openFileFromStream(renderer.getBeanStream(structure.getType(), renderer.getCurrentBean()
//							.getId()), text.getText());
//				}
//			}
//		});		
		
		createSelectorButton(renderer, composite, element, text,structure);
		createOpenButton(renderer, composite, element, text,structure);
		
		//TODO RAP
		//renderer.getToolkit().paintBordersFor(renderer.getParent());				
		if (parameters.getParameterBoolean(MANDATORY))
			renderer.addMandatoryAttribute(element.getCode());
		renderer.getRendererBinding().bindElement(element, text);

		renderer.addSaveListener(new IBeanMapChangedListener() {
			public void changed(BeanMapEvent event) {
				upload(renderer,structure);
			}
		});
	}

	public void dispose() {
		// Do nothing
	}

	private Composite createDefaultComposite(ISWTRenderer renderer, int horizontalSpan) {
		Composite composite = renderer.getToolkit().createComposite(renderer.getParent(), SWT.NONE);
		GridLayout gridLayout = new GridLayout(3, false);
		gridLayout.marginBottom = gridLayout.marginHeight = gridLayout.marginLeft = gridLayout.marginRight = gridLayout.marginTop = gridLayout.marginWidth = 0;
		composite.setLayout(gridLayout);
		if (renderer.getParent().getLayout() instanceof GridLayout)
			composite.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false, horizontalSpan, 1));
		return composite;
	}
	
	
	protected abstract void createSelectorButton(
			final ISWTRenderer renderer,final Composite composite,Element element,
			final Text receiver,final MetaDataEntity structure);
	
	protected abstract void createOpenButton(
			final ISWTRenderer renderer,final Composite composite,Element element,
			final Text receiver,final MetaDataEntity structure);
	
	protected void upload(final ISWTRenderer renderer,final MetaDataEntity structure){
		if (filePath != null){
			renderer.updateBeanStream(structure.getType(), renderer.getCurrentBean().getId(),
					new File(filePath));		
		}
	}
	
	
}
