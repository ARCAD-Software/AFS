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
package com.arcadsoftware.afs.client.macro.model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import com.arcadsoftware.afs.client.macro.internal.Activator;

public class CSVLines extends ArrayList<CSVLine> {

	/**
	 *
	 */
	private static final long serialVersionUID = -3533670590085071164L;

	private String[] headers = null;
	private int[] columnWidths = null;

	public void setHeaders(String[] headers) {
		this.headers = headers;
	}

	public String[] getHeaders() {
		return headers;
	}

	public void setColumnWidths(int[] columnWidths) {
		this.columnWidths = columnWidths;
	}

	public int[] getColumnWidths() {
		return columnWidths;
	}

	@Override
	public boolean add(CSVLine e) {
		for (int i = 0; i < headers.length; i++) {
			final String hdr = headers[i];
			final String value = e.get(hdr);
			final int max = Math.max(hdr.length(), value.length());
			if (max > columnWidths[i]) {
				columnWidths[i] = max;
			}
		}
		return super.add(e);
	}

	private static int[] createDefaultColumnWidths(int size) {
		final int[] widths = new int[size];
		for (int i = 0; i < size; i++) {
			widths[i] = -1;
		}
		return widths;
	}

	public static CSVLines fromCsv(File csvFile) {
		final CSVLines result = new CSVLines();

		try (BufferedReader csvReader = new BufferedReader(new FileReader(csvFile))) {
			String row;
			boolean firstRow = true;
			String[] headers = null;
			int[] widths = null;
			while ((row = csvReader.readLine()) != null) {
				if (firstRow) {
					headers = row.split(",");
					widths = createDefaultColumnWidths(headers.length);
					result.setHeaders(headers);
					result.setColumnWidths(widths);
					firstRow = false;
				} else {
					final CSVLine line = new CSVLine();
					final String[] data = row.split(",");
					for (int i = 0; i < headers.length; i++) {
						final String hdr = headers[i];
						final String value = data[i];
						line.put(hdr, value);
					}
					result.add(line);
				}
			}
		} catch (final IOException e) {
			Activator.getDefault().error(e.getLocalizedMessage(), e);
		}
		return result;
	}

}
