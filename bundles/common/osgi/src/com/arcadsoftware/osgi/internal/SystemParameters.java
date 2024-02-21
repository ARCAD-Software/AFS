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
package com.arcadsoftware.osgi.internal;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Base64;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.zip.CRC32;

import com.arcadsoftware.osgi.ISystemParameters;

public class SystemParameters implements ISystemParameters {

	private static final String ISO_8859_1 = "ISO-8859-1"; //$NON-NLS-1$
	private static final int BYTES_PER_INTEGER = Integer.SIZE / Byte.SIZE;
	
	private Activator activator;

	public SystemParameters(Activator activator) {
		super();
		this.activator = activator;
	}

	public String getSystemParameters() {
		String osn = System.getProperty("os.name", "none"); //$NON-NLS-1$ //$NON-NLS-2$
		String osa = System.getProperty("os.arch", "noarch"); //$NON-NLS-1$ //$NON-NLS-2$
		String osv = System.getProperty("os.version", "000"); //$NON-NLS-1$ //$NON-NLS-2$
		try {
			CRC32 crcosn = new CRC32();
			crcosn.update(osn.getBytes(ISO_8859_1));
			CRC32 crcosa = new CRC32();
			crcosa.update(osa.getBytes(ISO_8859_1));
			CRC32 crcosv = new CRC32();
			crcosv.update(osv.getBytes(ISO_8859_1));
			CRC32 crcmac = new CRC32();
			crcmac.update(getMacAddress().getBytes(ISO_8859_1)); //$NON-NLS-1$ //$NON-NLS-2$
			String sec = Base64.getEncoder().encodeToString(fourIntToBytes((int)crcosn.getValue(),(int)crcosa.getValue(),(int)crcosv.getValue(),(int)crcmac.getValue()));
			sec = sec.replace('+', '#');
			sec = sec.replace('/', '&');
			sec = sec.replace('=', '$');
			StringBuilder sb = new StringBuilder(sec.substring(0,22));
			sb.insert(6, '-');
			sb.insert(12, '-');
			sb.insert(19, '-');
			return sb.toString();
		} catch (UnsupportedEncodingException e) {
			activator.error(e.getLocalizedMessage(), e);
			return ""; //$NON-NLS-1$
		}
	}
	
	private String getMacAddress() {
		if (System.getProperty("java.version").startsWith("1.5")) { //$NON-NLS-1$ //$NON-NLS-2$
			String result = new MacAddress5().getMacAddress();
			if (result == null) {
				return MacAddress5.NOMACADDRESS;
			}
		}
		String result = null;
		try {
			result = new MacAddress6().getMacAddress();
		} catch (Throwable e) {
			activator.debug(e.getLocalizedMessage(), e);
		}
		if (result == null) {
			result = new MacAddress5().getMacAddress();
			if (result == null) {
				return MacAddress5.NOMACADDRESS;
			}
		}
		return result;
	}

	public boolean testParameters(String key, int parameters) {
		if (key == null) {
			return false;
		}
		try {
			key = key.replace("-", ""); //$NON-NLS-1$ //$NON-NLS-2$
			key = key.replace('#','+');
			key = key.replace('&','/');
			key = key.replace('$','=');
			if (key.length() == 22) {
				key = key + "=="; //$NON-NLS-1$
			}
			ByteBuffer bBuffer = ByteBuffer.wrap(Base64.getDecoder().decode(key));
			int osn = bBuffer.getInt();
			int osa = bBuffer.getInt();
			int osv = bBuffer.getInt();
			int mac = 0;
			if (bBuffer.hasRemaining()) {
				mac = bBuffer.getInt();
			}
			if ((parameters & PARAMETER_OSNAME) != 0) {
				CRC32 crc = new CRC32();
				crc.update(System.getProperty("os.name","none").getBytes(ISO_8859_1)); //$NON-NLS-1$ //$NON-NLS-2$
				if (osn != (int)crc.getValue()) {
					activator.warn(Messages.getString("SystemParameters.Invalid_test_name"), null); //$NON-NLS-1$
					return false;
				}
			}
			if ((parameters & PARAMETER_OSARCH) != 0) {
				CRC32 crc = new CRC32();
				crc.update(System.getProperty("os.arch","noarch").getBytes(ISO_8859_1)); //$NON-NLS-1$ //$NON-NLS-2$
				if (osa != (int)crc.getValue()) {
					activator.warn(Messages.getString("SystemParameters.Invalid_test_arch"), null); //$NON-NLS-1$
					return false;
				}
			}
			if ((parameters & PARAMETER_OSVERSION) != 0) {
				CRC32 crc = new CRC32();
				crc.update(System.getProperty("os.version","000").getBytes(ISO_8859_1)); //$NON-NLS-1$ //$NON-NLS-2$
				if (osv != (int)crc.getValue()) {
					activator.warn(Messages.getString("SystemParameters.Invalid_test_version"), null); //$NON-NLS-1$
					return false;
				}
			}
			if ((parameters & PARAMETER_MACADDRESS) != 0) {
				CRC32 crc = new CRC32();
				crc.update(getMacAddress().getBytes(ISO_8859_1)); //$NON-NLS-1$ //$NON-NLS-2$
				if (mac != (int)crc.getValue()) {
					activator.warn(Messages.getString("SystemParameters.Invalid_test_mac"), null); //$NON-NLS-1$
					return false;
				}
			}
			return true;
		} catch (UnsupportedEncodingException e) {
			activator.error(e.getLocalizedMessage(), e);
			return false;
		}
	}

	private byte[] fourIntToBytes(int val, int val2, int val3, int val4) {
		byte[] bArray = new byte[BYTES_PER_INTEGER * 4];
		ByteBuffer bBuffer = ByteBuffer.wrap(bArray);
		bBuffer.asIntBuffer().put(val);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(val2);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(val3);
		bBuffer.getInt();
		bBuffer.asIntBuffer().put(val4);
		return bArray;
	}

	public void storeParametersTest(String systemKey, int parameters) {
		synchronized (activator) {
			Dictionary<String, Object> properties = activator.getCurrentConfiguration();
			if (properties == null) {
				properties = new Hashtable<String, Object>();
			}
			properties.put(Activator.PROP_CONFIGPARAM, parameters);
			properties.put(Activator.PROP_CONFIGSYSTEMKEY, systemKey);
			activator.updateConfiguration(properties);
		}
	}
	
}
