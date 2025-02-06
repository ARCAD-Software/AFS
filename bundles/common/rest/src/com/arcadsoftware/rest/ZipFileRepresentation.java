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
package com.arcadsoftware.rest;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.restlet.data.Disposition;
import org.restlet.data.MediaType;
import org.restlet.representation.OutputRepresentation;

/**
 * This represation generate a ZIP fil of the given set of file, without using a local temporary file.
 * 
 * @author ARCAD Software
 */
public class ZipFileRepresentation extends OutputRepresentation {

	private final ArrayList<File> files;
	
	public ZipFileRepresentation(File... file) {
		this(MediaType.APPLICATION_ZIP, null, file);
	}

	public ZipFileRepresentation(String filename, File... file) {
		this(MediaType.APPLICATION_ZIP, filename, file);
	}

	public ZipFileRepresentation(MediaType mediatype, String filename, File... file) {
		super(mediatype);
        long md = 0;
        long s = 0;
        files = new ArrayList<File>();
        for (File f: file) {
        	if ((f.isFile()) && f.isFile()) {
        		files.add(f);
        		s += f.length();
        		if (f.lastModified() > md) {
        			md = f.lastModified();
        		}
        	}
        }
        setSize(s);
        Disposition disposition = new Disposition();
        if (filename != null) {
        	disposition.setFilename(filename);
        }
        if (md > 0) {
        	disposition.setModificationDate(new Date(md));
        }
    	disposition.setSize(s);
        setDisposition(disposition);
	}
	
	public ZipFileRepresentation(List<File> files) {
		this(MediaType.APPLICATION_ZIP, null, files);
	}

	public ZipFileRepresentation(String filename, List<File> files) {
		this(MediaType.APPLICATION_ZIP, filename, files);
	}

	public ZipFileRepresentation(MediaType mediatype, String filename, List<File> files) {
		super(mediatype);
        long md = 0;
        long s = 0;
        this.files = new ArrayList<File>();
        for (File f: files) {
        	if ((f.isFile()) && f.isFile()) {
        		this.files.add(f);
        		s += f.length();
        		if (f.lastModified() > md) {
        			md = f.lastModified();
        		}
        	}
        }
        setSize(s);
        Disposition disposition = new Disposition();
        if (filename != null) {
        	disposition.setFilename(filename);
        }
        if (md > 0) {
        	disposition.setModificationDate(new Date(md));
        }
    	disposition.setSize(s);
        setDisposition(disposition);
	}

	@Override
	public void write(OutputStream outputStream) throws IOException {
		byte[] buffer = new byte[1024];
		int length;
		try (ZipOutputStream zos = new ZipOutputStream(outputStream)) {
			for (File f: files) {
				try (FileInputStream fis = new FileInputStream(f)) {
					zos.putNextEntry(new ZipEntry(f.getName())); 
					while ((length = fis.read(buffer)) > 0) {
	                    zos.write(buffer, 0, length);
	                }
	                zos.closeEntry();
				}
			}
		}
		
	}

}
