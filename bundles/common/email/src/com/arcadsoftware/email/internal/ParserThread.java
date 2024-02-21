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
package com.arcadsoftware.email.internal;

import java.io.IOException;

class ParserThread extends Thread {
	HTMLParser parser;

	ParserThread(HTMLParser p) {
		parser = p;
	}

	public void run() { // convert pipeOut to pipeIn
		try {
			try { // parse document to pipeOut
				parser.HTMLDocument();
			} catch (ParseException e) {
				System.out.println(Messages.ParserThread_Aborted + e.getMessage());
			} catch (TokenMgrError e) {
				System.out.println(Messages.ParserThread_Aborted + e.getMessage());
			} finally {
				parser.pipeOut.close();
				synchronized (parser) {
					parser.summary.setLength(HTMLParser.SUMMARY_LENGTH);
					parser.titleComplete = true;
					parser.notifyAll();
				}
			}
		} catch (IOException e) {
			Activator.getInstance().error("Error during HTML Parsing.", e); //$NON-NLS-1$
		}
	}
}
