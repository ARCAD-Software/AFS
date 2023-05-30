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
package com.arcadsoftware.afs.client.reporting.ui.viewers;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTreeLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTreeViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.brands.AFSIcon;
import com.arcadsoftware.afs.client.reporting.Activator;
import com.arcadsoftware.afs.client.reporting.core.ReportCategory;
import com.arcadsoftware.afs.client.reporting.core.ReportHeader;



public class ReportHeaderViewer extends AbstractColumnedTreeViewer {

	public ReportHeaderViewer(Composite parent, int style) {
		super(parent, style);
	}



	@Override
	public String getIdentifier() {
		return null;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		ArcadColumns cols = new ArcadColumns();
		cols.add(new ArcadColumn("name",//$NON-NLS-1$
				Activator.resString("reportview.list.header.name.label"),ArcadColumn.VISIBLE,0,200,0));//$NON-NLS-1$
		cols.add(new ArcadColumn("description",//$NON-NLS-1$
				Activator.resString("reportview.list.header.description.label"),ArcadColumn.VISIBLE,1,200,1));//$NON-NLS-1$
		cols.add(new ArcadColumn("version",//$NON-NLS-1$
				Activator.resString("reportview.list.header.version.label"),ArcadColumn.VISIBLE,2,100,2));//$NON-NLS-1$
		return cols;
	}



	@Override
	public String getValue(Object element, int columnIndex) {
		if(element instanceof ReportHeader) {
			ReportHeader rh = (ReportHeader)element;
			switch (columnIndex) {
			case 0:	return rh.getName();
			case 1: return rh.getDescription();
			case 2: return rh.getVersion();
			default:
				break;
			}
		} else if(element instanceof ReportCategory) {
			ReportCategory rc = (ReportCategory)element;
			switch (columnIndex) {
			case 0:	return rc.getLabel();
			case 1: return ""; //$NON-NLS-1$	
			case 2: return ""; //$NON-NLS-1$
			default:
				break;
			}
		}
		return ""; //$NON-NLS-1$
	}

	public ReportHeader getSelected(){
		IStructuredSelection selection = (IStructuredSelection)this.getViewer().getSelection();
		if (!selection.isEmpty() && selection.getFirstElement() instanceof ReportHeader){
			return (ReportHeader)selection.getFirstElement();
		}
		return null;
	}



	@Override
	public AbstractColumnedTreeLabelProvider createTreeLabelProvider(
			AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTreeLabelProvider(viewer) {
			@Override
		    public Image getColumnImage(Object element, int columnIndex) {
				Image result = null;
				int actualIndex = viewer.getActualIndexFromDisplayedIndex(columnIndex);
		        if (actualIndex != -1) {
			        result = getActualImage(element,actualIndex);
			        if (result == null && columnIndex==0){
						if (element instanceof ReportCategory) {
							result = AFSIcon.REPORTS.image();
						}
						else if (element instanceof ReportHeader) {
							result = AFSIcon.REPORT.image();
						}
			        }
		        }
		        return result;
		    }
			
		};
	}
	
}
