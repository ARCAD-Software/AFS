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
package com.arcadsoftware.afs.client.macro.internal.ui.viewers;

import java.util.ArrayList;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Composite;

import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumn;
import com.arcadsoftware.aev.core.ui.columned.model.ArcadColumns;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.AbstractColumnedTableLabelProvider;
import com.arcadsoftware.aev.core.ui.labelproviders.columned.ColumnedDefaultTableLabelProvider;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedTableViewer;
import com.arcadsoftware.aev.core.ui.viewers.columned.AbstractColumnedViewer;
import com.arcadsoftware.afs.client.macro.model.CSVLine;
import com.arcadsoftware.afs.client.macro.model.CSVLines;

public class CsvTableViewer extends AbstractColumnedTableViewer {

	private final CSVLines content;

	public CsvTableViewer(Composite parent, int style, CSVLines content) {
		super(parent, style, false);
		this.content = content;
		init();
	}

	@Override
	protected void doOnSelectionChange(IStructuredSelection selection) {
	}

	protected ArrayList<Action> createActions() {
		final ArrayList<Action> result = new ArrayList<>();
		return result;
	}

	@Override
	public AbstractColumnedTableLabelProvider createTableLabelProvider(
			AbstractColumnedViewer viewer) {
		return new ColumnedDefaultTableLabelProvider(viewer);
	}

	@Override
	public String getValue(Object element, int columnIndex) {
		if (element instanceof CSVLine) {
			final CSVLine line = (CSVLine) element;
			final String header = content.getHeaders()[columnIndex];
			return line.get(header).replace("\"", "");
		}
		return "";
	}

	@Override
	public String getIdentifier() {
		return null;
	}

	@Override
	public ArcadColumns getReferenceColumns() {
		final ArcadColumns refColumns = new ArcadColumns();
		final String[] headers = content.getHeaders();
		for (int i = 0; i < headers.length; i++) {
			final String header = headers[i];
			final int width = content.getColumnWidths()[i];
			refColumns.add(new ArcadColumn(header + "id", formatHeader(header), ArcadColumn.VISIBLE, i, width * 10, i)); //$NON-NLS-1$
		}
		return refColumns;
	}

	private String formatHeader(String header) {
		String formattedHeader = "";
		final String[] segments = header.split("_");
		for (final String segment : segments) {
			formattedHeader = formattedHeader + (formattedHeader.length() == 0 ? "" : " ")
					+ segment.substring(0, 1).toUpperCase();
			if (segment.length() > 1) {
				formattedHeader = formattedHeader + segment.substring(1, segment.length()).toLowerCase();
			}
		}
		return formattedHeader;
	}

}
