package com.arcadsoftware.rest;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.restlet.data.Language;
import org.restlet.data.MediaType;
import org.restlet.representation.StringRepresentation;

/**
 * HTTP representation of a JSON Schema stored in a file.
 * 
 * @author ARCAD Software
 */
public class JSONSchemaRepresentation extends StringRepresentation {

    public static final MediaType APPLICATION_JSON_SCHEMA = MediaType.register(
            "application/schema+json", //$NON-NLS-1$
            "JSON Schema"); //$NON-NLS-1$
	
	private static String load(File file) {
		StringBuilder sb = new StringBuilder();
		FileInputStream fip;
		try {
			fip = new FileInputStream(file);
			try {
				byte[] buffer = new byte[1024]; // more than needed
				int x;
				while ((x = fip.read(buffer)) != -1) {
					sb.append(new String(buffer, 0, x));
				}
			} catch (IOException e) {
			} finally {
				try {
					fip.close();
				} catch (IOException e) {
				}
			}
		} catch (FileNotFoundException e) {
			return ""; //$NON-NLS-1$
		}
		return sb.toString();
	}
	
	public JSONSchemaRepresentation(File schema, Language language) {
		super(load(schema), APPLICATION_JSON_SCHEMA, language);
	}
	
	public JSONSchemaRepresentation(File schema) {
		this(schema, Language.ENGLISH_US);
	}

}
