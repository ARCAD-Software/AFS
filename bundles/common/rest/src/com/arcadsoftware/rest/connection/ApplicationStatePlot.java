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
package com.arcadsoftware.rest.connection;

import java.io.Serializable;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

/**
 * Information part about the application state returned by a IApplicationStateBroadcaster service.
 * 
 * <p>
 * This object may be serialized. Its content is relative to the user and language requested and should not be shared through different requests.
 * 
 * @see IApplicationStateBroadcaster
 * @author ARCAD Software
 */
public class ApplicationStatePlot implements Serializable, Comparable<ApplicationStatePlot>, Cloneable {
	
	private static final long serialVersionUID = -7502876256696564763L;

	public static enum PlotLevel {
		INFO,
		WARN,
		CRITICAL,
		BLOCKER;
	}

	private final String code;
	private final PlotLevel level;
	private final String hypertext;
	private final int lifeSpan;
	
	/**
	 * Create a contextual information plot.
	 * 
	 * @param code An unique identifier associated to this information subject. If multiple plot are broadcasted with same code then only one of the highest level will
	 * be presented.
	 * @param level The severity level of this plot. INFO may be ignored and BLOCKER may cause the client application to refuse to work with this application.
	 * @param hypertext A translated human readable message.
	 * @param lifeSpan The expected duration on this information, in minutes. zero or negative values are ignored.
	 */
	public ApplicationStatePlot(String code, PlotLevel level, String hypertext, int lifeSpan) {
		super();
		this.code = code;
		this.level = level;
		this.hypertext = hypertext;
		this.lifeSpan = lifeSpan;
	}

	/**
	 * An unique identifier associated to this information subject. If multiple plot are broadcasted with same code then only one of the highest level will
	 * be presented.
	 * 
	 * @return a non null identifier.
	 */
	public String getCode() {
		if (code == null) {
			return ""; //$NON-NLS-1$
		}
		return code;
	}
	
	/**
	 * The severity level of this plot. 
	 * 
	 * <p>
	 * INFO may be ignored and BLOCKER may cause the client application to refuse to work with this application.
	 * 
	 * @return the severity level, default is INFO.
	 */
	public PlotLevel getLevel() {
		if (level == null) {
			return PlotLevel.INFO;
		}
		return level;
	}
	
	/**
	 * A translated human readable message.
	 * 
	 * <p>
	 * A null message will avoid the presentation of this plot.
	 * 
	 * @return Human readable message. 
	 */
	public String getHypertext() {
		return hypertext;
	}
	
	/**
	 * The expected duration on this information, in minutes. zero or negative values are ignored.
	 * 
	 * <p>
	 * If this duration is unknown then return zero.
	 * 
	 * <p>
	 * Avoid to low value as it may necessary charge this application server.
	 * 
	 * @return the expected time (in minutes) of actualization of this plot.
	 */
	public int getLifeSpan() {
		return lifeSpan;
	}

	@Override
	public int compareTo(ApplicationStatePlot o) {
		if (o == null) {
			return 1;
		}
		if (!o.level.equals(level)) {
			return o.level.compareTo(level);
		}
		int i = getCode().compareToIgnoreCase(o.getCode());
		if (i == 0) {
			return getLevel().ordinal() - o.getLevel().ordinal();
		}
		return i;
	}

	@Override
	public int hashCode() {
		if (code == null) {
			return 0;
		}
		return code.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj instanceof ApplicationStatePlot) && // 
				getCode().equals(((ApplicationStatePlot) obj).getCode()) && //
				getLevel().equals(((ApplicationStatePlot) obj).getLevel());
	}

	@Override
	protected ApplicationStatePlot clone() {
		return new ApplicationStatePlot(code, level, hypertext, lifeSpan);
	}

	@Override
	public String toString() {
		if (code == null) {
			return hypertext;
		}
		if (hypertext == null) {
			return '[' + code + "] " + level.toString(); //$NON-NLS-1$
		}
		return '[' + code + "] " + level.toString() + " > " + hypertext; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		try {
			result.put("code", code);
			result.put("level", level.toString());
			result.put("text", hypertext);
		} catch (JSONException e) {}
		return result;
	}
}
