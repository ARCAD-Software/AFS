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
package com.arcadsoftware.mail;

import java.util.HashMap;
import java.util.Map.Entry;

/**
 * Extracted from http://www.iana.org
 * 
 * <p>
 * Creation Date: 17 nov. 2011
 */
public class MIME2FileExtension {

	private static final HashMap<String, String> FILEEXTENTIONMAP = new HashMap<String, String>(500);

	static {
		FILEEXTENTIONMAP.put("application/acad", "dwg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/xhtml+xml", "html"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/arj", "arj"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/base64", "mme"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/binhex", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/binhex4", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/book", "book"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/cdf", "cdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/clariscad", "ccad"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/commonground", "dp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/drafting", "drw"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/dsptype", "tsp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/dxf", "dxf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/envoy", "evy"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/excel", "xls"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/fractals", "fif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/freeloader", "frl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/futuresplash", "spl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/gnutar", "tgz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/groupwise", "vew"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/hlp", "hlp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/hta", "hta"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/i-deas", "unv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/iges", "igs"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/inf", "inf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/java", "class"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/java-byte-code", "class"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/lha", "lha"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/lzx", "lzx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mac-binary", "bin"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mac-binhex", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mac-binhex40", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mac-compactpro", "cpt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/macbinary", "bin"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/marc", "mrc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mbedlet", "mbd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mcad", "mcd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mime", "aps"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mspowerpoint", "ppt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/msword", "doc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/mswrite", "wri"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/netmc", "mcp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/octet-stream", "bin"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/oda", "oda"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pdf", "pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkcs-12", "p12"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkcs-crl", "crl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkcs10", "p10"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkcs7-mime", "p7m"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkcs7-signature", "p7s"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkix-cert", "crt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pkix-crl", "crl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/plain", "text"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/postscript", "ps"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/powerpoint", "ppt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/pro_eng", "prt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/ringing-tones", "rng"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/rtf", "rtf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/sdp", "sdp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/sea", "sea"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/set", "set"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/sla", "stl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/smil", "smi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/solids", "sol"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/sounder", "sdr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/step", "stp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/streamingmedia", "ssm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/toolbook", "tbk"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vda", "vda"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.fdf", "fdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.hp-hpgl", "hgl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.hp-pcl", "pcl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-excel", "xls"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-pki.certstore", "sst"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-pki.pko", "pko"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-pki.seccat", "cat"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-pki.stl", "stl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-powerpoint", "ppt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.ms-project", "mpp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.nokia.configuration-message", "ncm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.nokia.ringing-tone", "rng"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.rn-realmedia", "rm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.rn-realplayer", "rnx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.wap.wmlc", "wmlc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.wap.wmlscriptc", "wmlsc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vnd.xara", "web"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vocaltec-media-desc", "vmd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/vocaltec-media-file", "vmf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/wordperfect", "wpd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/wordperfect6.0", "w60"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/wordperfect6.1", "w61"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-123", "wk1"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-aim", "aim"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-authorware-bin", "aab"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-authorware-map", "aam"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-authorware-seg", "aas"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bcpio", "bcpio"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-binary", "bin"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-binhex40", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bsh", "bsh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bytecode.elisp (compiled elisp)", "elc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bytecode.python", "pyc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bzip", "bz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-bzip2", "bz2"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cdf", "cdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cdlink", "vcd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-chat", "chat"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cmu-raster", "ras"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cocoa", "cco"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-compactpro", "cpt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-compress", "z"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-compressed", "gz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-conference", "nsc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cpio", "cpio"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-cpt", "cpt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-csh", "csh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-deepv", "deepv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-director", "dcr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-dvi", "dvi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-elc", "elc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-envoy", "env"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-esrehber", "es"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-excel", "xls"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-frame", "mif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-freelance", "pre"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-gsp", "gsp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-gss", "gss"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-gtar", "gtar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-gzip", "gz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-hdf", "hdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-helpfile", "hlp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-httpd-imap", "imap"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-ima", "ima"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-internett-signup", "ins"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-inventor", "iv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-ip2", "ip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-java-class", "class"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-java-commerce", "jcm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-javascript", "js"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-koan", "skd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-ksh", "ksh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-latex", "ltx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lha", "lha"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lisp", "lsp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-livescreen", "ivy"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lotus", "wq1"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lotusscreencam", "scm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lzh", "lzh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-lzx", "lzx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mac-binhex40", "hqx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-macbinary", "bin"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-magic-cap-package-1.0", "mc$"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mathcad", "mcd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-meme", "mm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-midi", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mif", "mif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mix-transfer", "nix"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mplayer2", "asx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-msexcel", "xls"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-mspowerpoint", "ppt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-navi-animation", "ani"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-navidoc", "nvd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-navimap", "map"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-navistyle", "stl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-netcdf", "cdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-newton-compatible-pkg", "pkg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-nokia-9000-communicator-add-on-software", "aos"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-omc", "omc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-omcdatamaker", "omcd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-omcregerator", "omcr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pagemaker", "pm5"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pcl", "pcl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pixclscript", "plx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs10", "p10"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs12", "p12"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs7-certificates", "spc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs7-certreqresp", "p7r"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs7-mime", "p7m"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pkcs7-signature", "p7a"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-pointplus", "css"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-portable-anymap", "pnm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-project", "mpx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-qpro", "wb1"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-rtf", "rtf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sdp", "sdp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sea", "sea"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-seelogo", "sl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sh", "sh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-shar", "shar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-shockwave-flash", "swf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sit", "sit"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sprite", "spr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-stuffit", "sit"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sv4cpio", "sv4cpio"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-sv4crc", "sv4crc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-tar", "tar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-tbook", "tbk"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-tcl", "tcl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-tex", "tex"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-texinfo", "texinfo"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-troff", "t"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-troff-man", "man"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-troff-me", "me"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-troff-ms", "ms"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-troff-msvideo", "avi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-ustar", "ustar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-visio", "vsd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-vnd.audioexplosion.mzz", "mzz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-vnd.ls-xpix", "xpix"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-vrml", "vrml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-wais-source", "wsrc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-winhelp", "hlp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-wintalk", "wtk"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-world", "wrl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-wpwin", "wpd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-wri", "wri"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-x509-ca-cert", "cer"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-x509-user-cert", "crt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/x-zip-compressed", "zip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/xml", "xml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("application/zip", "zip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/aiff", "aif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/basic", "snd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/it", "it"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/make", "my"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/make.my.funk", "pfunk"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/mid", "rmi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/midi", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/mod", "mod"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/mpeg", "mpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/mpeg3", "mp3"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/nspaudio", "la"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/s3m", "s3m"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/tsp-audio", "tsi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/tsplayer", "tsp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/vnd.qcelp", "qcp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/voc", "voc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/voxware", "vox"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/wav", "wav"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-adpcm", "snd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-aiff", "aif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-au", "au"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-gsm", "gsm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-jam", "jam"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-liveaudio", "lam"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-mid", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-midi", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-mod", "mod"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-mpeg", "mp2"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-mpeg-3", "mp3"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-mpequrl", "m3u"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-nspaudio", "la"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-pn-realaudio", "rm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-pn-realaudio-plugin", "rmp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-psid", "sid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-realaudio", "ra"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-twinvq", "vqf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-twinvq-plugin", "vqe"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-vnd.audioexplosion.mjuicemediafile", "mjf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-voc", "voc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/x-wav", "wav"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("audio/xm", "xm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("chemical/x-pdb", "pdb"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("drawing/x-dwf (old)", "dwf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("i-world/i-vrml", "ivr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/bmp", "bmp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/cgm", "cgm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/cmu-raster", "ras"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/fif", "fif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/florian", "flo"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/g3fax", "g3"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/gif", "gif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/ief", "ief"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jpeg", "jpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jpg", "jpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jpm", "jpm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jpx", "jpx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jp2", "jp2"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/jutvision", "jut"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/naplps", "nap"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/pict", "pic"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/pjpeg", "jpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/png", "png"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/tiff", "tif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vasa", "mcf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.adobe.photoshop", "psd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.microsoft.icon", "ico"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.globalgraphics.pgb", "pgb"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.radiance", "hdr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.ms-modi", "mdi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.sealed.png", "spng"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.sealedmedia.softseal.gif", "sgif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.sealedmedia.softseal.jpg", "sjpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/svg+xml", "svg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/svg-xml", "svg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/svg", "svg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.dwg", "dwg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.dxf", "dxf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.fpx", "fpx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.net-fpx", "fpx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.rn-realflash", "rf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.rn-realpix", "rp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.svf", "svf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.wap.wbmp", "wbmp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/vnd.xiff", "xif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-cmu-raster", "ras"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-dwg", "dwg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-icon", "ico"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-jg", "art"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-jps", "jps"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-niff", "nif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-pcx", "pcx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-pict", "pct"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-portable-anymap", "pnm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-portable-bitmap", "pbm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-portable-graymap", "pgm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-portable-greymap", "pgm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-portable-pixmap", "ppm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-quicktime", "qif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-rgb", "rgb"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-tiff", "tif"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-windows-bmp", "bmp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-xbitmap", "xbm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-xbm", "xbm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-xpixmap", "xpm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-xwd", "xwd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/x-xwindowdump", "xwd"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/xbm", "xbm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("image/xpm", "xpm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("message/rfc822", "mhtml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("model/iges", "igs"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("model/vnd.dwf", "dwf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("model/vrml", "vrml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("model/x-pov", "pov"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("multipart/x-gzip", "gzip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("multipart/x-ustar", "ustar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("multipart/x-zip", "zip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("music/crescendo", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("music/x-karaoke", "kar"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("paleovu/x-pv", "pvu"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/alternative", "txt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/asp", "asp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/css", "css"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/html", "htm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/mcf", "mcf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/pascal", "pas"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/plain", "txt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/richtext", "rtf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/scriplet", "wsc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/sgml", "sgml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/tab-separated-values", "tsv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/uri-list", "uri"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/vnd.abc", "abc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/vnd.fmi.flexstor", "flx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/vnd.rn-realtext", "rt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/vnd.wap.wml", "wml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/vnd.wap.wmlscript", "wmls"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/webviewhtml", "htt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-asm", "asm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-audiosoft-intra", "aip"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-c", "c"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-component", "htc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-fortran", "for"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-h", "h"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-java-source", "java"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-la-asf", "lsx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-m", "m"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-pascal", "p"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script", "hlb"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.csh", "csh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.elisp", "el"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.guile", "scm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.ksh", "ksh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.lisp", "lsp"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.perl", "pl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.perl-module", "pm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.phyton", "py"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.rexx", "rexx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.scheme", "scm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.sh", "sh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.tcl", "tcl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.tcsh", "tcsh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-script.zsh", "zsh"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-server-parsed-html", "shtml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-setext", "etx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-sgml", "sgml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-speech", "talk"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-uil", "uil"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-uuencode", "uue"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/x-vcalendar", "vcs"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("text/xml", "xml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/animaflex", "afl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/avi", "avi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/avs-video", "avs"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/dl", "dl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/fli", "fli"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/gl", "gl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/mpeg", "mpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/msvideo", "avi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/quicktime", "mov"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/vdo", "vdo"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/vivo", "viv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/vnd.rn-realvideo", "rv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/vnd.vivo", "viv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/vosaic", "vos"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-amt-demorun", "xdr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-amt-showrun", "xsr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-atomic3d-feature", "fmf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-dl", "dl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-dv", "dv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-fli", "fli"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-gl", "gl"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-isvideo", "isu"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-motion-jpeg", "mjpg"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-mpeg", "mp3"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-mpeq2a", "mp2"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-ms-asf", "asf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-ms-asf-plugin", "asx"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-msvideo", "avi"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-qtc", "qtc"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-scm", "scm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("video/x-sgi-movie", "mv"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("windows/metafile", "wmf"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("www/mime", "mime"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-conference/x-cooltalk", "ice"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-music/x-midi", "mid"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-world/x-3dmf", "3dm"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-world/x-svr", "svr"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-world/x-vrml", "vrml"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("x-world/x-vrt", "vrt"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("xgl/drawing", "xgz"); //$NON-NLS-1$ //$NON-NLS-2$
		FILEEXTENTIONMAP.put("xgl/movie", "xmz"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Return a file extension, with dot (like ".bin"), corresponding to
	 * the given MIME Type.
	 * 
	 * @param mimeType
	 * @return
	 */
	public static String getFileExtension(String mimeType) {
		if ((mimeType == null) || (mimeType.length() == 0)) {
			return ".bin"; //$NON-NLS-1$
		}
		int i = mimeType.indexOf(';');
		if (i > -1) {
			mimeType = mimeType.substring(0, i);
		}
		String result = FILEEXTENTIONMAP.get(mimeType.trim().toLowerCase());
		if (result == null) {
			i = mimeType.lastIndexOf('/');
			if (i > -1) {
				result = mimeType.substring(i+1);
			} else {
				result = mimeType;
			}
			i = result.lastIndexOf('+');
			if (i > -1) {
				result = result.substring(i+1);
			}
			i = result.lastIndexOf('-');
			if (i > -1) {
				result = result.substring(i+1);
			}
			i = result.lastIndexOf('.');
			if (i > -1) {
				result = result.substring(i+1);
			}
		}
		return "." + result; //$NON-NLS-1$
	}

	/**
	 * Get a MIME type the correspond to the file extension.
	 * 
	 * @param ext a file extension, can be null, can start with a dot or not.
	 * @return always return a MIME type even if non extension is found.
	 */
	public static String getFileMimeTypeFromExtension(String ext) {
		if ((ext == null) || (ext.length() == 0)) {
			return "application/octect-stream"; //$NON-NLS-1$
		}
		if (ext.charAt(0) == '.') {
			ext = ext.substring(1);
			if (ext.length() == 0) {
				return "application/octect-stream"; //$NON-NLS-1$
			}
		}
		ext = ext.toLowerCase();
		for(Entry<String, String> e:FILEEXTENTIONMAP.entrySet()) {
			if (e.getValue().equals(ext)) {
				return e.getKey();
			}
		}
		return "application/octect-stream"; //$NON-NLS-1$
	}
}
