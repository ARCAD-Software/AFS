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

import java.io.File;
import java.io.InputStream;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;

import com.arcadsoftware.aev.core.ui.EvolutionCoreUIPlugin;
import com.arcadsoftware.client.editors.swtwidgets.internal.Messages;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.Element;
import com.arcadsoftware.metadata.MetaDataEntity;

public class DownloadSWTProvider extends AbstractDownloadSWTProvider {

	@Override
	protected void createSelectorButton(
			ISWTRenderer renderer, final Composite composite, Element element,
			final Text receiver,
			final MetaDataEntity structure) {
		final Button browseButton = renderer.getToolkit().createButton(composite, BUTTON_LABEL, SWT.PUSH);
		browseButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				final FileDialog dialog = new FileDialog(composite.getShell());
				dialog.setText(Messages.DownloadSWTProvider_FileDialogText);
				final String path = dialog.open();
				if ((path != null) && (path.length() > 0)) {
					filePath = path;
					receiver.setText(dialog.getFileName());
				}
			}
		});
		browseButton.setEnabled(!element.isReadonly());
	}

	@Override
	protected void createOpenButton(final ISWTRenderer renderer, Composite composite,
			Element element, final Text receiver, final MetaDataEntity structure) {
		final Button openButton = renderer.getToolkit().createButton(composite,
				Messages.DownloadSWTProvider_OpenFileButton,
				SWT.PUSH);
		openButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (filePath != null) {
					EvolutionCoreUIPlugin.getDefault().getFileManagerProvider().openFile(new File(filePath));
				} else {
					final InputStream is = renderer.getBeanStream(structure.getType(),
							renderer.getCurrentBean().getId());
					EvolutionCoreUIPlugin.getDefault().getFileManagerProvider().openFileFromStream(is,
							receiver.getText());
				}
			}
		});
	}

}
