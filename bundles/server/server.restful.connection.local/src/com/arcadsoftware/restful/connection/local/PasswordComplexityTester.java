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
package com.arcadsoftware.restful.connection.local;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

import org.restlet.data.Language;

import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.crypt.RandomGenerator;
import com.arcadsoftware.rest.connection.IPasswordComplexityTester;

public class PasswordComplexityTester implements IPasswordComplexityTester {

	private final Activator activator;
	private int passwordMin;
	private int passwordMax;
	private int passwordMinDigit;
	private int passwordMinAlpha;
	private int passwordMinChar;
	private int passwordMinNonAlpha;
	private int passwordDistLogin;
	private int passwordDistOld;
	private int passwordCharLogin;
	private int passwordLowerCase;
	private int passwordUpperCase;
	private int passwordCharOld;
	private int passwordRepeated;
	private HashSet<String> blacklist;
	
	protected PasswordComplexityTester(Activator activator) {
		super();
		this.activator = activator;
	}
	
	@Override
	public char[] generateAcceptablePassword(String login, char[] oldPassword) {
		for (int i = 0; i < 200; i++) {
			char[] pwd = genaux(login, oldPassword);
			if (pwd == null) {
				return null;
			}
			// The genaux method do not ensure the respect of "dist old", "dist login", "min diff" and "repeated" constraints.
			if (isPasswordAcceptable(login, oldPassword, pwd) == REASON_OK) {
				return pwd;
			}
		}
		return null;
	}

	private char[] genaux(String login, char[] oldPassword) {
		int l = passwordLowerCase + passwordUpperCase;
		if (l < passwordMinAlpha) {
			l = passwordMinAlpha;
		}
		l += passwordMinDigit + passwordMinNonAlpha;
		if (l < passwordMinChar) {
			l = passwordMinChar;
		}
		if (l < passwordMin) {
			l = passwordMin;
		}
		if (passwordMax > 3) {
			if (l > passwordMax) {
				return null; // invalid total of constraints !
			}
			l = RandomGenerator.randInteger(l, passwordMax);
		} else if (l > 0) {
			l = RandomGenerator.randInteger(l, l + (l >> 1));
		}
		if (l == 0) {
			// There is no constraints about password length.
			// We are going to generate a random password
			l = RandomGenerator.randInteger(12, 18);
		}
		char[] sb = new char[l];
		int i = 0;
		if (passwordMinNonAlpha > 0) {
			if (l < passwordMinNonAlpha) {
				RandomGenerator.randomCharsNonAlpha(sb, i, l);
				l = 0;
			} else {
				RandomGenerator.randomCharsNonAlpha(sb, i, passwordMinNonAlpha);
				l -= passwordMinNonAlpha;
				i += passwordMinNonAlpha;
			}
		} else {
			RandomGenerator.randomCharsNonAlpha(sb, i, 1);
			l -= 1;
			i++;
		}
		if (l > 0) {
			if (passwordMinDigit > 0) {
				if (l < passwordMinDigit) {
					RandomGenerator.randomCharsDigit(sb, i, l);
					l = 0;
				} else {
					RandomGenerator.randomCharsDigit(sb, i, passwordMinDigit);
					l -= passwordMinDigit;
					i += passwordMinDigit;
				}
			} else {
				RandomGenerator.randomCharsDigit(sb, i, 1);
				l -= 1;
				i++;
			}
		}
		if (l > 0) {
			if (passwordUpperCase > 0) {
				if (l < passwordUpperCase) {
					RandomGenerator.randomUpperCaseChars(sb, i, l);
					l = 0;
				} else {
					RandomGenerator.randomUpperCaseChars(sb, i, passwordUpperCase);
					l -= passwordUpperCase;
					i += passwordUpperCase;
				}
			}
		}
		if (l > 0) {
			if (passwordLowerCase > 0) {
				if (l < passwordLowerCase) {
					RandomGenerator.randomLowerCaseChars(sb, i, l);
					l = 0;
				} else {
					RandomGenerator.randomLowerCaseChars(sb, i, passwordLowerCase);
					l -= passwordLowerCase;
					i += passwordLowerCase;
				}
			}
		}
		if (l > 0) {
			RandomGenerator.randomChars(sb, i, l);
		}
		Crypto.scramble(sb);
		return sb;
	}

	@Override
	public String getTextualReason(int reason, Language language) {
		if (reason == 0) {
			return Activator.getMessage("ok", language); //$NON-NLS-1$
		}
		StringBuilder result = new StringBuilder(Activator.getMessage("Error", language)); //$NON-NLS-1$ 
		if ((reason & REASON_TOOSHORT) > 0) {
			result.append(Activator.getMessage("tooshort", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_TOOLONG) > 0) {
			result.append(Activator.getMessage("toolong", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHDIGIT) > 0) {
			result.append(Activator.getMessage("notenoughdigit", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHALPHA) > 0) {
			result.append(Activator.getMessage("notenoughalpha", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHDIFFCHAR) > 0) {
			result.append(Activator.getMessage("notenoughdiff", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHNONALPHA) > 0) {
			result.append(Activator.getMessage("notenoughnonalpha", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_TOOMANYLOGINCHAR) > 0) {
			result.append(Activator.getMessage("difflogin", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_SAMEASLOGIN) > 0) {
			result.append(Activator.getMessage("sameaslogin", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_SAMEASOLD) > 0) {
			result.append(Activator.getMessage("sameasoldpwd", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_TOOMANYOLDCHAR) > 0) {
			result.append(Activator.getMessage("diffoldpwd", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHLOWERCASE) > 0) {
			result.append(Activator.getMessage("notenoughlowercase", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_NOTENOUGHUPPERCASE) > 0) {
			result.append(Activator.getMessage("notenoughuppercase", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_TOOMANYREPEATING) > 0) {
			result.append(Activator.getMessage("toorepeating", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_BLACKLISTED) > 0) {
			result.append(Activator.getMessage("blacklisted", language)); //$NON-NLS-1$
		}
		if ((reason & REASON_UNKNOWN) > 0) {
			result.append(Activator.getMessage("misc", language)); //$NON-NLS-1$
		}
		return result.toString();
	}

	@Override
	public int isPasswordAcceptable(String login, char[] oldPassword, char[] newPassword) {
		int result = 0;
		if (newPassword == null) {
			return REASON_TOOSHORT;
		}
		// FIXME replace the String by a comparable object ! (same hash, same equals)
		if ((blacklist != null) && blacklist.contains(new String(newPassword))) {
			result |= REASON_BLACKLISTED;
		}
		int l = newPassword.length;
		if (l < passwordMin) {
			result |= REASON_TOOSHORT;
		}
		if ((passwordMax > 0) && (l > passwordMax)) {
			result |= REASON_TOOLONG;
		}
		if (passwordMinDigit > 0) {
			int i = 0;
			for (char c: newPassword) {
				if (Character.isDigit(c)) {
					i++;
				}
			}			
			if (i < passwordMinDigit) {
				result |= REASON_NOTENOUGHDIGIT;
			}
		}
		if (passwordMinAlpha > 0) {
			int i = 0;
			for (char c: newPassword) {
				if (Character.isAlphabetic(c)) {
					i++;
				}
			}			
			if (i < passwordMinAlpha) {
				result |= REASON_NOTENOUGHALPHA;
			}
		}
		if (passwordMinNonAlpha > 0) {
			int i = 0;
			for (char c: newPassword) {
				if (!Character.isAlphabetic(c) && !Character.isDigit(c)) {
					i++;
				}
			}
			if (i < passwordMinNonAlpha) {
				result |= REASON_NOTENOUGHNONALPHA;
			}
		}
		if (passwordMinChar > 0) {
			if (Crypto.diffCharCount(newPassword) < passwordMinChar) {
				result |= REASON_NOTENOUGHDIFFCHAR;
			}
		}
		if (passwordCharLogin > 0) {
			if (Crypto.countCharsInString(newPassword, login) > passwordCharLogin) {
				result |= REASON_TOOMANYLOGINCHAR;
			}
		}
		if (oldPassword != null) {
			if (passwordCharOld > 0) {
				if (Crypto.countCharsInString(newPassword, oldPassword) > passwordCharOld) {
					result |= REASON_TOOMANYOLDCHAR;
				}
			}
			if (passwordDistOld > 0) {
				if (Crypto.LD(newPassword, oldPassword) < passwordDistOld) {
					result |= REASON_SAMEASOLD;
				}
			}
		}
		if (passwordDistLogin > 0) {
			if (Crypto.LD(newPassword, login.toCharArray()) < passwordDistLogin) {
				result |= REASON_SAMEASLOGIN;
			}
		}
		if (passwordLowerCase > 0) {
			int i = 0;
			for (char c: newPassword) {
				if (Character.isLowerCase(c)) {
					i++;
				}
			}			
			if (i < passwordLowerCase) {
				result |= REASON_NOTENOUGHLOWERCASE;
			}
		}
		if (passwordUpperCase > 0) {
			int i = 0;
			for (char c: newPassword) {
				if (Character.isUpperCase(c)) {
					i++;
				}
			}			
			if (i < passwordUpperCase) {
				result |= REASON_NOTENOUGHUPPERCASE;
			}
		}
		if (passwordRepeated > 0) {
			int i = 0;
			char prev = (char) 0;
			for (char c: newPassword) {
				char cl = Character.toLowerCase(c);
				if (cl == prev) {
					i++;
					if (i > passwordRepeated) {
						break;
					}
				} else {
					prev = cl;
					i = 0;
				}
			}
			if (i > passwordRepeated) {
				result |= REASON_TOOMANYREPEATING;
			}
		}
		return result;
	}

	public int getPasswordMin() {
		return passwordMin;
	}

	public void setPasswordMin(int passwordMin) {
		this.passwordMin = passwordMin;
	}

	public int getPasswordMax() {
		return passwordMax;
	}

	public void setPasswordMax(int passwordMax) {
		this.passwordMax = passwordMax;
	}

	public int getPasswordMinDigit() {
		return passwordMinDigit;
	}

	public void setPasswordMinDigit(int passwordMinDigit) {
		this.passwordMinDigit = passwordMinDigit;
	}

	public int getPasswordMinAlpha() {
		return passwordMinAlpha;
	}

	public void setPasswordMinAlpha(int passwordMinAlpha) {
		this.passwordMinAlpha = passwordMinAlpha;
	}

	public int getPasswordMinChar() {
		return passwordMinChar;
	}

	public void setPasswordMinChar(int passwordMinChar) {
		this.passwordMinChar = passwordMinChar;
	}

	public int getPasswordMinNonAlpha() {
		return passwordMinNonAlpha;
	}

	public void setPasswordMinNonAlpha(int passwordMinNonAlpha) {
		this.passwordMinNonAlpha = passwordMinNonAlpha;
	}

	public int getPasswordDistLogin() {
		return passwordDistLogin;
	}

	public void setPasswordDistLogin(int passwordDistLogin) {
		this.passwordDistLogin = passwordDistLogin;
	}

	public int getPasswordDistOld() {
		return passwordDistOld;
	}

	public void setPasswordDistOld(int passwordDistOld) {
		this.passwordDistOld = passwordDistOld;
	}

	public int getPasswordCharLogin() {
		return passwordCharLogin;
	}

	public void setPasswordCharLogin(int passwordCharLogin) {
		this.passwordCharLogin = passwordCharLogin;
	}

	public int getPasswordLowerCase() {
		return passwordLowerCase;
	}

	public void setPasswordLowerCase(int passwordLowerCase) {
		this.passwordLowerCase = passwordLowerCase;
	}

	public int getPasswordUpperCase() {
		return passwordUpperCase;
	}

	public void setPasswordUpperCase(int passwordUpperCase) {
		this.passwordUpperCase = passwordUpperCase;
	}

	public int getPasswordCharOld() {
		return passwordCharOld;
	}

	public void setPasswordCharOld(int passwordCharOld) {
		this.passwordCharOld = passwordCharOld;
	}

	public int getPasswordRepeated() {
		return passwordRepeated;
	}

	public void setPasswordRepeated(int passwordRepeated) {
		this.passwordRepeated = passwordRepeated;
	}

	public void setBlacklist(final String parseStringParameter) {
		blacklist = null;
		if ((parseStringParameter != null) && !parseStringParameter.trim().isEmpty()) {
			final File file = new File(parseStringParameter);
			if (file.isFile()) {
				new Thread(new Runnable() {
					@Override
					public void run() {
						blacklist = new HashSet<String>(1000);
						try (FileReader fr = new FileReader(file)) {
							try (BufferedReader br = new BufferedReader(fr)) {
								String s = br.readLine();
								while (s != null) {
									if (!s.isEmpty()) {
										blacklist.add(s);
									}
									s = br.readLine();
								}
							}
						} catch (IOException e) {
							activator.error(e);
						}
					}
				}).start();
			}
		}
	}

}
