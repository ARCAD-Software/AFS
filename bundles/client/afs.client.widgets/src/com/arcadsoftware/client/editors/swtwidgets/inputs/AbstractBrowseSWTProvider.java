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
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.GET_FILE_PATH;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.LABEL;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.MANDATORY;
import static com.arcadsoftware.client.editors.swtwidgets.IConstants.TWO_POINTS;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IInputSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

/**
 * 
 */
public abstract class AbstractBrowseSWTProvider implements IInputSWTProvider {

	private static final String BUTTON_LABEL = "..."; //$NON-NLS-1$
	protected String filePath;
	protected boolean getFilePath = true;

	public void create(final ISWTRenderer renderer, final ILayoutParameters parameters, Element element,
			final MetaDataEntity structure) {
		getFilePath = parameters.getParameterBoolean(GET_FILE_PATH);
		
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

		Button browseButton = renderer.getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
		browseButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileInfo fileInfo = getFilename(composite);
				if (fileInfo!=null){
					filePath = fileInfo.getAbsoluteFilename();
					if (getFilePath){
						text.setText(fileInfo.getAbsoluteFilename());
					} else {
						text.setText(fileInfo.getRelativeFilename());
					}
				}					
//				FileDialog dialog = new FileDialog(composite.getShell());
//				dialog.setText(Messages.DownloadSWTProvider_FileDialogText);
//				String path = dialog.open();
//				if (path != null && path.length() > 0) {
//					filePath = path;
//					if (getFilePath)
//						text.setText(filePath);
//					else
//						text.setText(dialog.getFileName());
//				}
			}
		});

		browseButton.setEnabled(!element.isReadonly());
		//TODO RAP
		//renderer.getToolkit().paintBordersFor(renderer.getParent());
		if (parameters.getParameterBoolean(MANDATORY))
			renderer.addMandatoryAttribute(element.getCode());
		renderer.getRendererBinding().bindElement(element, text);

//		renderer.addSaveListener(new IBeanMapChangedListener() {
//			public void changed(BeanMapEvent event) {
//				if (filePath != null){
//					
//					renderer.updateBeanStream(structure.getType(), renderer.getCurrentBean().getId(),
//							new File(filePath));
//				}
//			}
//		});
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
	
	
	protected abstract FileInfo getFilename(Composite parent);
	
	
}
