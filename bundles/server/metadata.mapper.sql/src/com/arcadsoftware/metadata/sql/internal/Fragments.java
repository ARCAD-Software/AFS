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
package com.arcadsoftware.metadata.sql.internal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Properties;
import java.util.Map.Entry;

/**
 *
 */
public class Fragments {

	private static final String BUNDLE_NAME = Fragments.class.getPackage().getName() + ".fragments"; //$NON-NLS-1$

	public Object undelete_val;
	public String true_cond;
	public String false_cond;
	public Object true_val;
	public Object false_val;
	public String equal;
	public String and;
	public String columnsep;
	public String asid;
	public String as;
	public String trunc_string;
	public String concat_string;
	public String concat;
	public String quote;
	public String join;
	public String join_inner;
	public String exists;
	public String partial;
	public String partialall;
	public String partialorder;
	public String partialallorder;
	public String count;
	public String select;
	public String selectall;
	public String selectorder;
	public String selectallorder;
	public String create;
	public String insert_hard; 
	public String update;
	public String delete;
	public String delete_update;
	public String deleteex;
	public String delete_hard;
	public String delete_hardex;
	public String undelete;
	public String undelete_update;
	public String dateformat;
	public String greater;
	public String datefunction;
	public String parin;
	public String parout;
	public String lower;
	public String contain;
	public String lowercase;
	public String endwith;
	public String greaterorequal;
	public String isnull;
	public String istrue;
	public String lowerorequal;
	public String not;
	public String or;
	public String startwith;
	public String tablealias;
	public String orderdesc;
	public String orderasc;
	public String equaldeltrue;
	public String equaldelfalse;
	public String param;
	public String paramex; 
	public String param2;
	public String paramset; 
	public String equalignorecase;
	public String distinct;
	public String truncateCols;
	public String alterColsSize;
	public String concat_days;
	public String updateex;
	public String paramequal;
	public String count_distinct;
	public String update_join;
	public String delete_val;
	public String notintoselect;
	public String inset;
	public String recursive;
	public String recursive_alt;
	public String select_const;
	public String id;
	public String isnullex;
	public String recursive_link;
	public String recursive_linkdel;
	public String source;
	public String dest;
	
	public Fragments(String name) {
		// Reset this class...
		load(BUNDLE_NAME);
		// Add specific declarations...
		if ((name != null) && (name.length() > 0)) {
			load(BUNDLE_NAME + '.' + name);
		}
	}
	
	private void load(final String bundleName) {
		InputStream input = Fragments.class.getClassLoader().getResourceAsStream(bundleName.replace('.', '/') + ".properties"); //$NON-NLS-1$
		if (input == null) {
			Activator.getInstance().debug(String.format(Messages.Fragments_NotFound, bundleName));
		} else {
			Properties properties = new Properties();
			try {
				properties.load(input);
			} catch (IOException e) {
				Activator.getInstance().error(Messages.Fragments_Invalid + bundleName, e);
			}
			Field[] fieldArray = Fragments.class.getDeclaredFields();
			HashMap<String,Field> fields = new HashMap<String,Field>(fieldArray.length * 2);
			for (Field field:fieldArray) {
				fields.put(field.getName(), field);
			}
			for(Entry<Object,Object> e: properties.entrySet()) {
				Field field = fields.get(e.getKey());
				if (field == null) {
					Activator.getInstance().debug(Messages.Fragments_Unknown + e.getKey());
				} else if ((field.getModifiers() & Modifier.PUBLIC) != 0) {
					try {
						if (field.getName().endsWith("_val")) {
							String val = e.getValue().toString().trim();
							try {
								field.set(this, Integer.valueOf(val));
							} catch (NumberFormatException err) {
								if ("true".equalsIgnoreCase(val)) { //$NON-NLS-1$
									field.set(this, Boolean.TRUE);
								} else if ("false".equalsIgnoreCase(val)) { //$NON-NLS-1$
									field.set(this, Boolean.FALSE);
								} else {
									field.set(this, e.getValue());
								}
							} catch (IllegalArgumentException er) {
								field.set(this, val);
							}
						} else {
							field.set(this, e.getValue());
						}
					} catch (Exception ex) {
						Activator.getInstance().error(Messages.Fragments_InternalError + e.getKey(), ex);
					}
				}
			}
		}
	}
	
}
