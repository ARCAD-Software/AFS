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
package com.arcadsoftware.cli.output.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;

import com.arcadsoftware.cli.core.services.AbstractService;
import com.arcadsoftware.cli.output.AbstractOutputManager;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class JSonOutputManager extends AbstractOutputManager  {
	
	public JSonOutputManager() {
		super();		
	}
	
	public JSonOutputManager(AbstractService service, File outputFile) {
		super(service, outputFile);		
	}

	@Override
	public void generateOutput() {
		super.generateOutput();
		String outputFilename = outputFile.getAbsolutePath();
		if (!outputFile.exists()) {
			outputFile.getParentFile().mkdirs();
		}
		GsonBuilder builder = new GsonBuilder(); 
		builder.setPrettyPrinting();
		Gson gson = builder.create(); 
		try (Writer writer = new OutputStreamWriter(new FileOutputStream(outputFilename) , StandardCharsets.UTF_8)){						
			writer.write(gson.toJson(root));
		}
		catch (Exception e) {}
	}
}