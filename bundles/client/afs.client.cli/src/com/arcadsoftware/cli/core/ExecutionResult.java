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
package com.arcadsoftware.cli.core;



public class ExecutionResult {

	private boolean succeed;
	private int exitCode;
	
	public ExecutionResult(boolean succeed, int exitCode){
		this.succeed = succeed;
		this.exitCode = exitCode; 
	}
	
	public int getExitCode() {
		return exitCode;
	}
	
	public boolean isSucceed() {
		return succeed;
	}
	
	public void setExitCode(int exitCode) {
		this.exitCode = exitCode;
	}
	
	public void setSucceed(boolean succeed) {
		this.succeed = succeed;
	}


}
