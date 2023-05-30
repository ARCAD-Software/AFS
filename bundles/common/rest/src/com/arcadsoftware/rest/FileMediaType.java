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
package com.arcadsoftware.rest;

import java.util.Hashtable;

import org.restlet.data.MediaType;

/**
 * This helper class can be used to guess the MediaType of a file just by its Extension code.
 * 
 */
public abstract class FileMediaType {
	
	/**
	 * JavaScript representation Media type.
	 * <p>
	 * Mediatype: text/javascript
	 * 
	 * @see MediaType.APPLICATION_JAVASCRIPT
	 */
	public static final MediaType TEXT_JAVASCRIPT = MediaType.register("text/javascript", "Javascript document"); //$NON-NLS-1$ //$NON-NLS-2$
	
	private static final Hashtable<String, MediaType> mediaext = new Hashtable<String, MediaType>(100);
	
	static {
		mediaext.put("cab", MediaType.APPLICATION_CAB); //$NON-NLS-1$
		mediaext.put("xls", MediaType.APPLICATION_EXCEL); //$NON-NLS-1$
		mediaext.put("xlt", MediaType.APPLICATION_EXCEL); //$NON-NLS-1$
		mediaext.put("tar", MediaType.APPLICATION_GNU_TAR); //$NON-NLS-1$
		mediaext.put("gz", MediaType.APPLICATION_GNU_ZIP); //$NON-NLS-1$
		mediaext.put("jar", MediaType.APPLICATION_JAVA_ARCHIVE); //$NON-NLS-1$
		mediaext.put("pdf", MediaType.APPLICATION_PDF); //$NON-NLS-1$
		mediaext.put("rtf", MediaType.APPLICATION_RTF); //$NON-NLS-1$
		mediaext.put("ps", MediaType.APPLICATION_POSTSCRIPT); //$NON-NLS-1$
		mediaext.put("eps", MediaType.APPLICATION_POSTSCRIPT); //$NON-NLS-1$
		mediaext.put("pps", MediaType.APPLICATION_POWERPOINT); //$NON-NLS-1$
		mediaext.put("ppt", MediaType.APPLICATION_POWERPOINT); //$NON-NLS-1$
		mediaext.put("mpp", MediaType.APPLICATION_PROJECT); //$NON-NLS-1$
		mediaext.put("sea", MediaType.APPLICATION_STUFFIT); //$NON-NLS-1$
		mediaext.put("sit", MediaType.APPLICATION_STUFFIT); //$NON-NLS-1$
		mediaext.put("doc", MediaType.APPLICATION_WORD); //$NON-NLS-1$
		mediaext.put("zip", MediaType.APPLICATION_ZIP); //$NON-NLS-1$
		mediaext.put("odt", new MediaType("application/vnd.oasis.opendocument.text")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("ods", new MediaType("application/vnd.oasis.opendocument.spreadsheet")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odp", new MediaType("application/vnd.oasis.opendocument.presentation")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odg", new MediaType("application/vnd.oasis.opendocument.graphics")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odc", new MediaType("application/vnd.oasis.opendocument.chart")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odf", new MediaType("application/vnd.oasis.opendocument.formula")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odb", new MediaType("application/vnd.oasis.opendocument.database")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odi", new MediaType("application/vnd.oasis.opendocument.image")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("odm", new MediaType("application/vnd.oasis.opendocument.text-master")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("ott", new MediaType("application/vnd.oasis.opendocument.text-template")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("ots", new MediaType("application/vnd.oasis.opendocument.spreadsheet-template")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("otp", new MediaType("application/vnd.oasis.opendocument.presentation-template")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("otg", new MediaType("application/vnd.oasis.opendocument.graphics-template")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("wks", new MediaType("application/vnd.ms-works")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("hlp", new MediaType("application/winhlp")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("z", new MediaType("application/x-compress")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("tgz", new MediaType("application/x-compressed")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("rar", new MediaType("application/x-rar-compressed")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("7z", new MediaType("application/x-7z-Compressed")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("wri", new MediaType("application/x-mswrite")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("mid", MediaType.AUDIO_MIDI); //$NON-NLS-1$
		mediaext.put("mpga", MediaType.AUDIO_MPEG); //$NON-NLS-1$
		mediaext.put("mp2", MediaType.AUDIO_MPEG); //$NON-NLS-1$
		mediaext.put("mp3", MediaType.AUDIO_MPEG); //$NON-NLS-1$
		mediaext.put("ram", MediaType.AUDIO_REAL); //$NON-NLS-1$
		mediaext.put("ram", MediaType.AUDIO_REAL); //$NON-NLS-1$
		mediaext.put("wav", MediaType.AUDIO_WAV); //$NON-NLS-1$
		mediaext.put("exe", MediaType.APPLICATION_OCTET_STREAM); //$NON-NLS-1$
		mediaext.put("bin", MediaType.APPLICATION_OCTET_STREAM); //$NON-NLS-1$
		mediaext.put("dll", MediaType.APPLICATION_OCTET_STREAM); //$NON-NLS-1$
		mediaext.put("bmp", MediaType.IMAGE_BMP); //$NON-NLS-1$
		mediaext.put("gif", MediaType.IMAGE_GIF); //$NON-NLS-1$
		mediaext.put("ico", MediaType.IMAGE_ICON); //$NON-NLS-1$
		mediaext.put("jpg", MediaType.IMAGE_JPEG); //$NON-NLS-1$
		mediaext.put("jpe", MediaType.IMAGE_JPEG); //$NON-NLS-1$
		mediaext.put("jpeg", MediaType.IMAGE_JPEG); //$NON-NLS-1$
		mediaext.put("png", MediaType.IMAGE_PNG); //$NON-NLS-1$
		mediaext.put("svg", MediaType.IMAGE_SVG); //$NON-NLS-1$
		mediaext.put("svgz", MediaType.IMAGE_SVG); //$NON-NLS-1$
		mediaext.put("tif", MediaType.IMAGE_TIFF); //$NON-NLS-1$
		mediaext.put("tiff", MediaType.IMAGE_TIFF); //$NON-NLS-1$
		mediaext.put("htm", MediaType.TEXT_HTML); //$NON-NLS-1$
		mediaext.put("html", MediaType.TEXT_HTML); //$NON-NLS-1$
		mediaext.put("xhtml", MediaType.APPLICATION_XHTML); //$NON-NLS-1$
		mediaext.put("txt", MediaType.TEXT_PLAIN); //$NON-NLS-1$
		mediaext.put("mpg", MediaType.VIDEO_MPEG); //$NON-NLS-1$
		mediaext.put("mpeg", MediaType.VIDEO_MPEG); //$NON-NLS-1$
		mediaext.put("mp4", MediaType.VIDEO_MP4); //$NON-NLS-1$
		mediaext.put("qt", MediaType.VIDEO_QUICKTIME); //$NON-NLS-1$
		mediaext.put("wmv", MediaType.VIDEO_WMV); //$NON-NLS-1$
		mediaext.put("avi", new MediaType("video/x-msvideo")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("xvid", new MediaType("video/x-xvid")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("divx", new MediaType("video/x-divx")); //$NON-NLS-1$ //$NON-NLS-2$
		mediaext.put("dtd", MediaType.APPLICATION_XML_DTD); //$NON-NLS-1$
		mediaext.put("xml", MediaType.APPLICATION_XML); //$NON-NLS-1$
		mediaext.put("js", MediaType.APPLICATION_JAVASCRIPT); //$NON-NLS-1$
		mediaext.put("json", MediaType.APPLICATION_JSON); //$NON-NLS-1$
		mediaext.put("css", MediaType.TEXT_CSS); //$NON-NLS-1$
		mediaext.put("log", MediaType.TEXT_PLAIN); //$NON-NLS-1$
	}


	/**
	 * Return the most appropriate MediaType, according to the file extension.
	 * 
	 * @param fileName The file name.
	 * @return The MediaType of the file, default return {@link MediaType#APPLICATION_OCTET_STREAM}.
	 */
	public static MediaType guess(String fileName) {
		int i = fileName.lastIndexOf('.');
		if (i > 0) {
			MediaType result = mediaext.get(fileName.substring(i+1).toLowerCase());
			if (result != null) {
				return result;
			}
		}
		return MediaType.APPLICATION_OCTET_STREAM;
	}
}
