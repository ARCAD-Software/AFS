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
package com.arcadsoftware.afs.client.core.ui.composites;

import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Display;

import com.arcadsoftware.metadata.criteria.StartCriteria;
import com.arcadsoftware.metadata.xml.XmlCriteriaStream;

public class SearchKeyListener extends KeyAdapter {

	private class EraseTask extends TimerTask {

		private boolean active = false;

		@Override
		public void run() {
			synchronized (buffer) {
				if (count > 500) {
					Display.getDefault().asyncExec(
							new Runnable() {

								@Override
								public void run() {
									parentSearcher.search();
									buffer = "";
									count = 0;
								}

							});
					active = false;
					cancel();
				} else {
					count += 50;
				}
			}
		}

		public boolean isActive() {
			return active;
		}

		public void setActive(boolean active) {
			this.active = active;
		}

	}

	private int count = 0;
	private AbstractSearchListComposite parentSearcher = null;
	private String attribute = null;
	private final Timer timer;
	private String buffer = " ";
	private EraseTask task = null;

	public SearchKeyListener(AbstractSearchListComposite parentSearcher, String attribute) {
		this.attribute = attribute;
		task = new EraseTask();
		timer = new Timer();
		this.parentSearcher = parentSearcher;
	}

	public String getAttribute() {
		return attribute;
	}

	@Override
	public void keyPressed(KeyEvent e) {
		if (e.character != 0x0) {
			if (buffer.equalsIgnoreCase(" ")) {
				buffer = "";
			}
			buffer = buffer + e.character;
			count = 0;
			if (!task.isActive()) {
				task = new EraseTask();
				task.setActive(true);
				timer.schedule(task, 0, 50);
			}
		}
	}

	public synchronized String createSearchClause(String sc) {
		if (buffer.length() > 0) {
			if (buffer.equalsIgnoreCase(" ")) {
				return sc;
			}
			final XmlCriteriaStream x = new XmlCriteriaStream();
			final StartCriteria equalType = new StartCriteria(attribute, buffer);
			if (sc != null) {
				final String clause = "<and>" + sc + x.toXML(equalType) + "</and>";
				return clause;
			} else {
				final String clause = x.toXML(equalType);
				return clause;
			}

		} else {
			return null;
		}
	}

}
