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
package com.arcadsoftware.editor.implementation.swt;

import java.util.HashMap;

/**
 * Map used to decode "thml" like color codes into real colors !
 */
public class ColorMapTable {

	static public final ColorMapTable TABLE = new ColorMapTable();

	private final HashMap<String, String> colorNamesMap;

	/**
	 *
	 */
	private ColorMapTable() {
		colorNamesMap = new HashMap<>(147);
		colorNamesMap.put("aliceBlue", "#F0F8FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("aqua", "#00FFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("aquamarine", "#7FFFD4"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("azure", "#F0FFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("beige", "#F5F5DC"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("bisque", "#FFE4C4"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("black", "#000000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("blanchedalmond", "#FFEBCD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("blue", "#0000FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("blueviolet", "#8A2BE2"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("brown", "#A52A2A"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("burlywood", "#DEB887"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("cadetblue", "#5F9EA0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("chartreuse", "#7FFF00"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("chocolate", "#D2691E"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("coral", "#FF7F50"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("cornflowerblue", "#6495ED"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("cornsilk", "#FFF8DC"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("crimson", "#DC143C"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("cyan", "#00FFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkblue", "#00008B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkcyan", "#008B8B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkgoldenrod", "#B8860B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkgray", "#A9A9A9"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkgrey", "#A9A9A9"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkgreen", "#006400"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkkhaki", "#BDB76B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkmagenta", "#8B008B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkolivegreen", "#556B2F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkorange", "#FF8C00"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkorchid", "#9932CC"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkred", "#8B0000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darksalmon", "#E9967A"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkseagreen", "#8FBC8F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkslateblue", "#483D8B"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkslategray", "#2F4F4F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkslategrey", "#2F4F4F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkturquoise", "#00CED1"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("darkviolet", "#9400D3"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("deeppink", "#FF1493"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("deepskyblue", "#00BFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("dimgray", "#696969"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("dimgrey", "#696969"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("dodgerblue", "#1E90FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("firebrick", "#B22222"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("floralwhite", "#FFFAF0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("forestgreen", "#228B22"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("fuchsia", "#FF00FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("gainsboro", "#DCDCDC"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("ghostwhite", "#F8F8FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("gold", "#FFD700"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("goldenrod", "#DAA520"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("gray", "#808080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("grey", "#808080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("green", "#008000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("greenyellow", "#ADFF2F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("honeydew", "#F0FFF0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("hotpink", "#FF69B4"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("indianred", "#CD5C5C"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("indigo", "#4B0082"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("ivory", "#FFFFF0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("khaki", "#F0E68C"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lavender", "#E6E6FA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lavenderblush", "#FFF0F5"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lawngreen", "#7CFC00"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lemonchiffon", "#FFFACD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightblue", "#ADD8E6"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightcoral", "#F08080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightcyan", "#E0FFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightgoldenrodyellow", "#FAFAD2"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightgray", "#D3D3D3"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightgrey", "#D3D3D3"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightgreen", "#90EE90"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightpink", "#FFB6C1"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightsalmon", "#FFA07A"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightseagreen", "#20B2AA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightskyblue", "#87CEFA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightslategray", "#778899"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightslategrey", "#778899"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightsteelblue", "#B0C4DE"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lightyellow", "#FFFFE0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("lime", "#00FF00"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("limegreen", "#32CD32"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("linen", "#FAF0E6"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("magenta", "#FF00FF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("maroon", "#800000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumaquamarine", "#66CDAA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumblue", "#0000CD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumorchid", "#BA55D3"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumpurple", "#9370D8"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumseagreen", "#3CB371"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumslateblue", "#7B68EE"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumspringgreen", "#00FA9A"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumturquoise", "#48D1CC"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mediumvioletred", "#C71585"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("midnightblue", "#191970"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mintcream", "#F5FFFA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("mistyrose", "#FFE4E1"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("moccasin", "#FFE4B5"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("navajowhite", "#FFDEAD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("navy", "#000080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("oldlace", "#FDF5E6"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("olive", "#808000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("olivedrab", "#6B8E23"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("orange", "#FFA500"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("orangered", "#FF4500"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("orchid", "#DA70D6"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("palegoldenrod", "#EEE8AA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("palegreen", "#98FB98"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("paleturquoise", "#AFEEEE"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("palevioletred", "#D87093"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("papayawhip", "#FFEFD5"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("peachpuff", "#FFDAB9"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("peru", "#CD853F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("pink", "#FFC0CB"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("plum", "#DDA0DD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("powderblue", "#B0E0E6"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("purple", "#800080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("red", "#FF0000"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("rosybrown", "#BC8F8F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("royalblue", "#4169E1"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("saddlebrown", "#8B4513"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("salmon", "#FA8072"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("sandybrown", "#F4A460"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("seagreen", "#2E8B57"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("seashell", "#FFF5EE"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("sienna", "#A0522D"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("silver", "#C0C0C0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("skyblue", "#87CEEB"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("slateblue", "#6A5ACD"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("slategray", "#708090"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("slategrey", "#708090"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("snow", "#FFFAFA"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("springgreen", "#00FF7F"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("steelblue", "#4682B4"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("tan", "#D2B48C"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("teal", "#008080"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("thistle", "#D8BFD8"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("tomato", "#FF6347"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("turquoise", "#40E0D0"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("violet", "#EE82EE"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("wheat", "#F5DEB3"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("white", "#FFFFFF"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("whitesmoke", "#F5F5F5"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("yellow", "#FFFF00"); //$NON-NLS-1$ //$NON-NLS-2$
		colorNamesMap.put("yellowgreen", "#9ACD32"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public String get(String color) {
		return colorNamesMap.get(color);
	}
}
