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
package com.arcadsoftware.afs.client.core.thread;

/**
 * Only exists because we need a Thread subclass for RAP.
 *  
 * @author ARCAD Software
 */
public class UIThread extends Thread {
	
	
	public UIThread(Runnable target) {
		super(target);
	}
	
	public UIThread() {
		super();
	}
	
	public UIThread(String name){
		super(name);
	}
	
	public void shutDown(){
		//to be overridden
	}

}
