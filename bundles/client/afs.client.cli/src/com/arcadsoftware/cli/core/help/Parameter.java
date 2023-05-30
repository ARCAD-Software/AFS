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
package com.arcadsoftware.cli.core.help;

public class Parameter {

	private String modifier;
	private String help;
	private boolean mandatory;
	private String defaultValue;
	private String sample;
	
	
	public Parameter (String modifer, String help, boolean mandatory, String defaultValue, String sample) {
		this.modifier = modifer;
		this.mandatory = mandatory;
		this.help = help;
		this.defaultValue = defaultValue;
		this.sample = sample;
	}


	public String getModifier() {
		return modifier;
	}


	public void setModifier(String modifier) {
		this.modifier = modifier;
	}


	public String getHelp() {
		return help;
	}


	public void setHelp(String help) {
		this.help = help;
	}


	public boolean isMandatory() {
		return mandatory;
	}


	public void setMandatory(boolean mandatory) {
		this.mandatory = mandatory;
	}


	public String getDefaultValue() {
		return defaultValue;
	}


	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}
	
	public String getSample() {
		return sample;
	}
	
	public void setSample(String sample) {
		this.sample = sample;
	}
	
	public String displayedHelp(){
		String s =	
			"+ Modifier         : ["+modifier+"]\n"+
	        "    . Description  : "+help+"\n"+	
			"    . Mandatory    :"+(isMandatory()?"X":"");
		if (defaultValue.length()>0) {
			s = s+"\n    .Default Value : "+defaultValue;
		}
		if (sample.length()>0) {
			s = s+"\n    .Sample        : "+sample;
		}		
		return s;
	}
			
	
	
}
