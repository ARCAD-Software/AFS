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
package com.arcadsoftware.afs.client.server.admin.common.ui.settings.viewers;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.server.admin.common.Activator;
import com.arcadsoftware.afs.client.server.admin.common.core.model.category.CategoryWrapper;
import com.arcadsoftware.afs.framework.ui.images.ImageManager;

public class SectionViewer extends AbstractColumnedTreeViewer {

	public SectionViewer(Composite parent, int style) {
		super(parent, style);
	}


	@Override
	public String getValue(Object element, int columnIndex) {
		if (columnIndex==0){
			if (element instanceof CategoryWrapper){
				return  ((CategoryWrapper)element).getLabel();
			}
		}
		return null;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		ArcadColumns cols = new ArcadColumns();
		cols.add(new ArcadColumn("category",Activator.resString("settingseditor.category.header.name"),ArcadColumn.VISIBLE,0,250,0));  //$NON-NLS-1$//$NON-NLS-2$
		return cols;
	}
	
	
	@Override
	public String getIdentifier() {
		return null;
	}

	public CategoryWrapper getSelectedCategory(){
		IStructuredSelection selection = getSelection();
		if (!selection.isEmpty()) {//TODO [SSC] vérifier le cast
			return (CategoryWrapper)selection.getFirstElement();
		}
		return null;
	}

	@Override
	public AbstractColumnedTreeLabelProvider createTreeLabelProvider(
			AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTreeLabelProvider(viewer){
			/**
			 * 
			 */
			private static final long serialVersionUID = 4747861543215483962L;

			protected Image getImage(String key) {
				//return Activator.getInstance().getImage(key);
				return ImageManager.getInstance().getImage(key);
			}
		};
	}
	
	
}
