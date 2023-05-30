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
package com.arcadsoftware.client.editors.swtwidgets.linestylers;

import java.io.IOException;
import java.io.StringReader;
import java.util.Hashtable;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

public class GroovyLineStyler implements LineStyleListener {

	private static final String[] KEYWORDS = { "abstract", "boolean", "break", "byte", "case", "catch", "char", "class", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
		"continue", "default", "do", "double", "else", "extends", "false", "final", "finally", "float", "for", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$ //$NON-NLS-11$
		"if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
		"package", "private", "protected", "public", "return", "short", "static", "super", "switch", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
		"synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
		"each", "find", "findAll" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	private static final String[] LANGUAGES_ELEMENTS = { "?.", "?:", ".@", ".", "->", "[]", "[:]" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
	
	public static final int EOF = -1;
	public static final int EOL = 10;
	public static final int WORD = 0;
	public static final int WHITE = 1;
	public static final int KEY = 2;
	public static final int COMMENT = 3;
	public static final int STRING = 5;
	public static final int OTHER = 6;
	public static final int NUMBER = 7;
	public static final int API = 8;
	public static final int LE = 9;
	public static final int MAXIMUM_TOKEN = 10;
	
	GroovyScanner scanner;
	int[] tokenColors;
	int[] tokenStyles;
	Color[] colors;
	Vector<int[]> blockComments;

	public GroovyLineStyler(String[] apis) {
		super();
		initialize();
		scanner = new GroovyScanner(apis);
		blockComments = new Vector<int[]>();
	}

	private Color getColor(int type) {
		if (type < 0 || type >= tokenColors.length) {
			return null;
		}
		return colors[tokenColors[type]];
	}

	private int getFontStyle(int type) {
		if (type < 0 || type >= tokenStyles.length) {
			return SWT.NORMAL;
		}
		return tokenStyles[type];
	}

	boolean inBlockComment(int start, int end) {
		for (int i = 0; i < blockComments.size(); i++) {
			int[] offsets = blockComments.elementAt(i);
			// start of comment in the line
			if ((offsets[0] >= start) && (offsets[0] <= end))
				return true;
			// end of comment in the line
			if ((offsets[1] >= start) && (offsets[1] <= end))
				return true;
			if ((offsets[0] <= start) && (offsets[1] >= end))
				return true;
		}
		return false;
	}

	protected void initialize() {
		Display display = Display.getDefault();
		colors = new Color[] { new Color(display, new RGB(0, 0, 0)), // 
				new Color(display, new RGB(63, 127, 95)), //  comment
				new Color(display, new RGB(42, 0, 255)), //  string
				new Color(display, new RGB(127, 0, 85)), //  key
				new Color(display, new RGB(100, 100, 100)), // API
		};
		tokenColors = new int[MAXIMUM_TOKEN];
		tokenColors[WORD] = 0;
		tokenColors[WHITE] = 0;
		tokenColors[KEY] = 3;
		tokenColors[COMMENT] = 1;
		tokenColors[STRING] = 2;
		tokenColors[OTHER] = 0;
		tokenColors[NUMBER] = 0;
		tokenColors[API] = 4;
		tokenColors[LE] = 0;
		tokenStyles = new int[MAXIMUM_TOKEN];
		tokenStyles[WORD] = SWT.NORMAL;
		tokenStyles[WHITE] = SWT.NORMAL;
		tokenStyles[KEY] = SWT.BOLD;
		tokenStyles[COMMENT] = SWT.NORMAL;
		tokenStyles[STRING] = SWT.NORMAL;
		tokenStyles[OTHER] = SWT.NORMAL;
		tokenStyles[NUMBER] = SWT.BOLD;
		tokenStyles[API] = SWT.BOLD;
		tokenStyles[LE] = SWT.BOLD;
	}

	void disposeColors() {
		for (int i = 0; i < colors.length; i++) {
			colors[i].dispose();
		}
	}

	/**
	 * Event.detail line start offset (input) Event.text line text (input) LineStyleEvent.styles Enumeration of
	 * StyleRanges, need to be in order. (output) LineStyleEvent.background line background color (output)
	 */
	public void lineGetStyle(LineStyleEvent event) {
		Vector<StyleRange> styles = new Vector<StyleRange>();
		int token;
		StyleRange lastStyle;
		// If the line is part of a block comment, create one style for the entire line.
		if (inBlockComment(event.lineOffset, event.lineOffset + event.lineText.length())) {
			styles.addElement(new StyleRange(event.lineOffset, event.lineText.length(), getColor(COMMENT), null, getFontStyle(COMMENT)));
			event.styles = new StyleRange[styles.size()];
			styles.copyInto(event.styles);
			return;
		}
		Color defaultFgColor = ((Control) event.widget).getForeground();
		scanner.setRange(event.lineText);
		token = scanner.nextToken();
		while (token != EOF) {
			if (token == OTHER) {
				// do nothing for non-colored tokens
			} else if (token != WHITE) {
				Color color = getColor(token);
				int fontStyle = getFontStyle(token);
				// Only create a style if the token color is different than the
				// widget's default foreground color and the token's style is not
				// bold. Keywords are bolded.
				if (color!=null) {
					if ((!color.equals(defaultFgColor)) || (fontStyle != SWT.NORMAL)) {
						StyleRange style = new StyleRange(scanner.getStartOffset() + event.lineOffset, scanner.getLength(),
								color, null);
						style.fontStyle = fontStyle;
						if (styles.isEmpty()) {
							styles.addElement(style);
						} else {
							// Merge similar styles. Doing so will improve performance.
							lastStyle = (StyleRange) styles.lastElement();
							if (lastStyle.similarTo(style) && (lastStyle.start + lastStyle.length == style.start)) {
								lastStyle.length += style.length;
							} else {
								styles.addElement(style);
							}
						}
					}
				}
			} else if ((!styles.isEmpty()) && ((lastStyle = (StyleRange) styles.lastElement()).fontStyle == SWT.BOLD)) {
				int start = scanner.getStartOffset() + event.lineOffset;
				lastStyle = (StyleRange) styles.lastElement();
				// A font style of SWT.BOLD implies that the last style
				// represents a java keyword.
				if (lastStyle.start + lastStyle.length == start) {
					// Have the white space take on the style before it to
					// minimize the number of style ranges created and the
					// number of font style changes during rendering.
					lastStyle.length += scanner.getLength();
				}
			}
			token = scanner.nextToken();
		}
		event.styles = new StyleRange[styles.size()];
		styles.copyInto(event.styles);
	}

	public void parseBlockComments(String text) {
		blockComments = new Vector<int[]>();
		StringReader buffer = new StringReader(text);
		int ch;
		boolean blkComment = false;
		int cnt = 0;
		int[] offsets = new int[2];
		boolean done = false;

		try {
			while (!done) {
				switch (ch = buffer.read()) {
				case -1: {
					if (blkComment) {
						offsets[1] = cnt;
						blockComments.addElement(offsets);
					}
					done = true;
					break;
				}
				case '/': {
					ch = buffer.read();
					if ((ch == '*') && (!blkComment)) {
						offsets = new int[2];
						offsets[0] = cnt;
						blkComment = true;
						cnt++;
					} else {
						cnt++;
					}
					cnt++;
					break;
				}
				case '*': {
					if (blkComment) {
						ch = buffer.read();
						cnt++;
						if (ch == '/') {
							blkComment = false;
							offsets[1] = cnt;
							blockComments.addElement(offsets);
						}
					}
					cnt++;
					break;
				}
				default: {
					cnt++;
					break;
				}
				}
			}
		} catch (IOException e) {
			// ignore errors
		}
	}

	/**
	 * A simple fuzzy scanner for Java
	 */
	public class GroovyScanner {

		protected Hashtable<String, Integer> fgKeys;
		protected StringBuilder fBuffer;
		protected String fDoc;
		protected int fPos;
		protected int fEnd;
		protected int fStartToken;
		protected boolean fEofSeen = false;

		public GroovyScanner(String[] apis) {
			fBuffer = new StringBuilder();
			initialize(apis);
		}

		/**
		 * Returns the ending location of the current token in the document.
		 */
		public final int getLength() {
			return fPos - fStartToken;
		}

		/**
		 * Initialize the lookup table.
		 */
		void initialize(String[] apis) {
			fgKeys = new Hashtable<String, Integer>();
			if (apis != null) {
				Integer k = new Integer(API);
				for (int i = 0; i < apis.length; i++) {
					fgKeys.put(apis[i], k);
				}
			}
			Integer k = new Integer(KEY);
			for (int i = 0; i < KEYWORDS.length; i++) {
				fgKeys.put(KEYWORDS[i], k);
			}
			k = new Integer(LE);
			for (int i = 0; i < LANGUAGES_ELEMENTS.length; i++) {
				fgKeys.put(LANGUAGES_ELEMENTS[i], k);
			}
		}

		/**
		 * Returns the starting location of the current token in the document.
		 */
		public final int getStartOffset() {
			return fStartToken;
		}

		/**
		 * Returns the next lexical token in the document.
		 */
		public int nextToken() {
			int c;
			fStartToken = fPos;
			while (true) {
				switch (c = read()) {
				case EOF:
					return EOF;
				case '/': // comment
					c = read();
					if (c == '/') {
						while (true) {
							c = read();
							if ((c == EOF) || (c == EOL)) {
								unread(c);
								return COMMENT;
							}
						}
					}
					unread(c);
					return OTHER;
				case '\'': // char const
					while (true) {
						c = read();
						switch (c) {
						case '\'':
							return STRING;
						case EOF:
							unread(c);
							return STRING;
						case '\\':
							c = read();
							break;
						}
					}

				case '"': // string
					while (true) {
						c = read();
						switch (c) {
						case '"':
							return STRING;
						case EOF:
							unread(c);
							return STRING;
						case '\\':
							c = read();
							break;
						}
					}

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					do {
						c = read();
					} while (Character.isDigit((char) c));
					unread(c);
					return NUMBER;
				default:
					if (Character.isWhitespace((char) c)) {
						do {
							c = read();
						} while (Character.isWhitespace((char) c));
						unread(c);
						return WHITE;
					}
					if (Character.isJavaIdentifierStart((char) c)) {
						fBuffer.setLength(0);
						do {
							fBuffer.append((char) c);
							c = read();
						} while (Character.isJavaIdentifierPart((char) c));
						unread(c);
						Integer i = (Integer) fgKeys.get(fBuffer.toString());
						if (i != null)
							return i.intValue();
						return WORD;
					}
					return OTHER;
				}
			}
		}

		/**
		 * Returns next character.
		 */
		protected int read() {
			if (fPos <= fEnd) {
				return fDoc.charAt(fPos++);
			}
			return EOF;
		}

		public void setRange(String text) {
			fDoc = text;
			fPos = 0;
			fEnd = fDoc.length() - 1;
		}

		protected void unread(int c) {
			if (c != EOF)
				fPos--;
		}
	}

}
