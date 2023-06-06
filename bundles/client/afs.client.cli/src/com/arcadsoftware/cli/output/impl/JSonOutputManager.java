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
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.core.ClassLoaderReference;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;

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
		// [ML) The JSON format may have changed here (v2023.7.0). Add aliases if required...
		XStream xs = new XStream(null, new JsonHierarchicalStreamDriver(), new ClassLoaderReference(JSonOutputManager.class.getClassLoader()));
		try (Writer writer = new OutputStreamWriter(new FileOutputStream(outputFilename) , StandardCharsets.UTF_8)) {						
			writer.write(xs.toXML(root));
		}
		catch (Exception e) {}
	}
}