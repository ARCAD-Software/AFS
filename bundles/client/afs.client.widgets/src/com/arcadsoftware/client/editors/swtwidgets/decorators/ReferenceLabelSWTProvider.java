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
package com.arcadsoftware.client.editors.swtwidgets.decorators;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

import com.arcadsoftware.beanmap.IBeanMap;
import com.arcadsoftware.client.editors.swtwidgets.IConstants;
import com.arcadsoftware.client.editors.swtwidgets.ReferenceObservableValue;
import com.arcadsoftware.editor.ILayoutParameters;
import com.arcadsoftware.editor.swt.IDecoratorSWTProvider;
import com.arcadsoftware.editor.swt.ISWTRenderer;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.MetaDataFormater;

public class ReferenceLabelSWTProvider implements IDecoratorSWTProvider {

	private static final String ANTI_SLASH_POINT = "\\."; //$NON-NLS-1$
	private static final String REFERENCE = "reference"; //$NON-NLS-1$

	private ISWTRenderer renderer;
	private String[] attributes;
	private MetaDataEntity[] structures;
	private MetaDataEntity rootStructure;
	private String defaultText;
	private String formatString;
	private MetaDataFormater formatter;
	private Label label;
	private Thread structuresLoading;
	private boolean translate;

	@Override
	public Widget create(ISWTRenderer renderer, ILayoutParameters parameters, MetaDataEntity structure) {
		this.renderer = renderer;
		rootStructure = structure;
		translate = parameters.getParameterBoolean(IConstants.TRANSLATE);
		final String lbx = parameters.getParameter(IConstants.LABEL, ""); //$NON-NLS-1$
		// Base label
		if (lbx.length() > 0) {
			renderer.getToolkit().createLabel(renderer.getParent(), renderer.getLocalizedMessage(lbx));
			renderer.getToolkit().createLabel(renderer.getParent(), IConstants.TWO_POINTS);
		}
		defaultText = renderer.getLocalizedMessage(parameters.getParameter(IConstants.DEFAULT, "")); //$NON-NLS-1$
		label = renderer.getToolkit().createLabel(renderer.getParent(), defaultText);

		final boolean fillHorizontal = parameters.getParameterBoolean(IConstants.FILL_HORIZONTAL, true);

		if ((renderer.getParent().getLayout() instanceof GridLayout)) {
			// fill the grid line.
			final int nbCol = ((GridLayout) renderer.getParent().getLayout()).numColumns;
			final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
			gd.grabExcessHorizontalSpace = fillHorizontal;
			if (lbx.length() == 0) {
				gd.horizontalSpan = nbCol;
			}
			label.setLayoutData(gd);
			// label.setLayoutData(new GridData(GridData.BEGINNING, GridData.BEGINNING, fillHorizontal, false,
			// ((GridLayout) renderer.getParent().getLayout()).numColumns, 1));
		}
		final String atts = parameters.getParameter(REFERENCE, ""); //$NON-NLS-1$
		formatString = parameters.getParameter(IConstants.FORMAT, defaultText);
		// Informations loading
		if (atts.length() > 0) {
			attributes = atts.split(ANTI_SLASH_POINT);
			if ((attributes == null) || (attributes.length == 0)) {
				return label;
			}
			final MetaDataAttribute element = rootStructure.getAttribute(attributes[0]);
			if (element == null) {
				return label;
			}
			loadStructures();
			// Data Binding
			renderer.getRendererBinding().getBinding().bindValue(new ReferenceObservableValue(this),
					renderer.getRendererBinding().getObservableAttribute(element), null, null);
			// This binding is a passive binding it do not need to be checked by
			// the binding manager.
		}
		return label;
	}

	/**
	 * Load the structural informations asynchronously.
	 */
	private void loadStructures() {
		structures = new MetaDataEntity[attributes.length];
		structuresLoading = new Thread(new Runnable() {
			@Override
			public void run() {
				// Load Structures list.
				MetaDataEntity lastStructure = rootStructure;
				for (int i = 0; i < attributes.length; i++) {
					final MetaDataAttribute element = lastStructure.getAttribute(attributes[i]);
					final MetaDataEntity refEntity = renderer.getStructure(element);
					if ((element == null) || (refEntity == null) || label.isDisposed()) {
						// The chain do not bring us to a BeanMap chain...
						structures[0] = null;
						// We cancel the process.
						return;
					}
					structures[i] = renderer.getStructure(element);
					lastStructure = structures[i];
				}
				// Create the final formatter.
				formatter = new MetaDataFormater(formatString, structures[attributes.length - 1]);
			}
		});
		structuresLoading.start();
	}

	public boolean isStructureLoaded() {
		return (structuresLoading == null) || !structuresLoading.isAlive();
	}

	public boolean isReady() {
		return isStructureLoaded() && //
				(structures != null) && //
				(structures.length > 0) && //
				(structures[0] != null) && //
				(formatter != null) && //
				(!label.isDisposed());
	}

	@Override
	public void dispose() {
		// We should tell the loading thread to stop immediately.
	}

	public void resetLabel() {
		if (isReady()) {
			label.setText(defaultText);
			label.pack();
		}
	}

	public void format(IBeanMap value) {
		if (isReady()) {
			final String key = formatter.format(value);
			final String s;
			if (translate) {
				s = renderer.getLocalizedMessage(key);
			} else {
				s = key;
			}

			label.getDisplay().asyncExec(
					new Runnable() {
						@Override
						public void run() {
							if (!label.isDisposed()) {
								label.setText(s);
								label.pack();
							}
						}
					});

			// PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			// public void run() {
			// if (!label.isDisposed()) {
			// label.setText(s);
			// label.pack();
			// }
			// }
			// });
		}
	}

	public ISWTRenderer getRenderer() {
		return renderer;
	}

	public String[] getAttributes() {
		return attributes;
	}

	public MetaDataEntity[] getStructures() {
		return structures;
	}

	public MetaDataEntity getRootStructure() {
		return rootStructure;
	}

	public boolean isDisposed() {
		return (label == null) || label.isDisposed();
	}

}
