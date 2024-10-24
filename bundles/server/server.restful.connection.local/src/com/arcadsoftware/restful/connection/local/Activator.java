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

import java.util.ArrayList;
import java.util.Date;
import java.util.Dictionary;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.osgi.framework.console.CommandInterpreter;
import org.eclipse.osgi.framework.console.CommandProvider;
import org.osgi.framework.BundleContext;
import org.osgi.util.tracker.ServiceTracker;
import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.crypt.Crypto;
import com.arcadsoftware.metadata.AbstractMapperService;
import com.arcadsoftware.metadata.IMetaDataDeleteListener;
import com.arcadsoftware.metadata.IMetaDataModifyListener;
import com.arcadsoftware.metadata.IMetaDataSelectionListener;
import com.arcadsoftware.metadata.MetaDataAttribute;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.criteria.DeletedCriteria;
import com.arcadsoftware.metadata.criteria.EqualCriteria;
import com.arcadsoftware.metadata.criteria.NotCriteria;
import com.arcadsoftware.osgi.AbstractConfiguredActivator;
import com.arcadsoftware.rest.MultiLanguageMessages;
import com.arcadsoftware.rest.connection.IConnectionCache;

public class Activator extends AbstractConfiguredActivator implements CommandProvider {

	protected static final String LOCALAUTH = "localauth"; //$NON-NLS-1$
	protected static final String LOCALAUTH_LOGIN = "login"; //$NON-NLS-1$
	protected static final String LOCALAUTH_LOCKED = "locked"; //$NON-NLS-1$
	protected static final String LOCALAUTH_ISLOCKED = "islocked"; //$NON-NLS-1$
	protected static final String LOCALAUTH_PASSWORD = "password"; //$NON-NLS-1$
	protected static final String LOCALAUTH_PWDUPDATE = "pwdupdate"; //$NON-NLS-1$
	protected static final String LOCALAUTH_USERID = "user"; //$NON-NLS-1$
	protected static final String LOCALAUTH_ALLATTRIBUTES = LOCALAUTH_LOGIN + ' ' + LOCALAUTH_LOCKED + ' ' + // 
			LOCALAUTH_PASSWORD + ' ' + LOCALAUTH_PWDUPDATE + ' ' + LOCALAUTH_USERID; //
	private static final String HTTP_MESSAGES = Activator.class.getPackage().getName() + ".clientmessages"; //$NON-NLS-1$
	private static final String PROP_PasswordMin = "password.min"; //$NON-NLS-1$
	private static final String PROP_PasswordMax = "password.max"; //$NON-NLS-1$
	private static final String PROP_PasswordMinDigit = "password.mindigit"; //$NON-NLS-1$
	private static final String PROP_PasswordMinAlpha = "password.minalpha"; //$NON-NLS-1$
	private static final String PROP_PasswordMinChar = "password.minchar"; //$NON-NLS-1$
	private static final String PROP_PasswordMinNonAlpha = "password.minnonalpha"; //$NON-NLS-1$
	private static final String PROP_PasswordMinLowerCase = "password.minlowercase"; //$NON-NLS-1$
	private static final String PROP_PasswordMinUpperCase = "password.minuppercase"; //$NON-NLS-1$
	private static final String PROP_PasswordDistLogin = "password.distlogin"; //$NON-NLS-1$
	private static final String PROP_PasswordDistOld = "password.distold"; //$NON-NLS-1$
	private static final String PROP_PasswordCharLogin = "password.charlogin"; //$NON-NLS-1$
	private static final String PROP_PasswordCharOld = "password.charold"; //$NON-NLS-1$
	private static final String PROP_PasswordRepeated = "password.minrepeated"; //$NON-NLS-1$
	private static final String PROP_BLACKLIST = "password.blacklist"; //$NON-NLS-1$
	private static final String KEY_LOCKCOUNT_MAX = "lockcount"; //$NON-NLS-1$
	private static final String PROP_PWDDURATION = "pwddelay"; //$NON-NLS-1$
	private static final String PROP_CASESENSITIVE = "casesensitive"; //$NON-NLS-1$
	private static final String PROP_ADMINACCOUNT = "admin.uid"; //$NON-NLS-1$
	private static final String PROP_ADMINLOCK = "admin.unlock.delay"; //$NON-NLS-1$
	private static final String LIST_PUBLIC_PROPS = KEY_LOCKCOUNT_MAX + ' ' + PROP_CASESENSITIVE + ' ' + PROP_PWDDURATION;
	private static final boolean DEFAULT_CASESENSITIVE = true;
	private static final int PWD_DURATION = 3650; // in days = 10 years
	private static final int LOCKCOUNT_MAX = 5; // Valeur par défaut du nombre de tentative.
	// CETTE VALEUR DOIT RESTER A 5 PAR DEFAUT PARCE QUE LES BROWSERS FONT 
	// SYSTEMATIQUEMENT UNE TENTATIVE AVANT D'OUVRIR LA DIALOGUE DE CONNECTION
	// ET TELECHARGENT DIVERS OBJETS EN PARALELLE.
	// De plus ce paramètre étant dans laconfiguration il n'y a AUCUNE raison de le modifier ici.

	private final ConcurrentHashMap<Integer, Timer> unlockTimers = new ConcurrentHashMap<Integer, Timer>();
	private int pwdDuration = PWD_DURATION;
	private boolean casesensitive = DEFAULT_CASESENSITIVE;
	private int maxLockCount = LOCKCOUNT_MAX;
	private volatile ArrayList<Integer> adminUIDs;
	private volatile long unlockDelay;
	private volatile HashMap<Integer, BeanMap> authCacheId;
	private volatile HashMap<String, BeanMap> authCacheLogin;
	private PasswordComplexityTester tester;
	private ServiceTracker<IConnectionCache, IConnectionCache> connectionCacheTracker;
	private MultiLanguageMessages messages;
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start(BundleContext context) throws Exception {
		tester = new PasswordComplexityTester(this);
		super.start(context);
		messages = new MultiLanguageMessages(HTTP_MESSAGES, Activator.class.getClassLoader());
		// Register an authentification service...
		registerService(LocalAuthentificationService.clazz, new LocalAuthentificationService(this), LocalAuthentificationService.ENTITYNAME, LOCALAUTH);
		// Register a service usable for password testing
		registerService(PasswordComplexityTester.clazz, tester);
		// Register a branch for password modifications.
		registerService(SecureBranch.clazz, new SecureBranch(this), SecureBranch.properties(SecureBranch.SECUREDBRANCH));
		// Register itself as a console command provider.
		registerService(CommandProvider.class.getName(), this);
		// Register LocalAuth modification listener (set locked property correctly and manage password limit date).
		registerService(IMetaDataModifyListener.class, new LocalAuthModifyListener(this), IMetaDataModifyListener.PROP_TYPE, LOCALAUTH);
		registerService(IMetaDataDeleteListener.class, new LocalAuthDeleteListener(this), IMetaDataDeleteListener.PROP_TYPE, LOCALAUTH);
		registerService(IMetaDataSelectionListener.class, new LocalAuthSelectionListener(this), IMetaDataDeleteListener.PROP_TYPE, LOCALAUTH);
		// Connection Cache access.
		connectionCacheTracker = new ServiceTracker(context, IConnectionCache.class, null);
		connectionCacheTracker.open();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		connectionCacheTracker.close();
		tester = null;
		for (Timer t: unlockTimers.values()) {
			if (t != null) {
				t.cancel();
			}
		}
		unlockTimers.clear();
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean initializeConfiguration(@SuppressWarnings("rawtypes") Dictionary properties) {
		boolean result = false;
		if ((properties.get(PROP_PUBLIC_PROPS) == null) || (!LIST_PUBLIC_PROPS.equals(properties.get(PROP_PUBLIC_PROPS).toString()))) {
			properties.put(PROP_PUBLIC_PROPS, LIST_PUBLIC_PROPS);
			result = true;
		}
		if (properties.get(PROP_PasswordMin) == null) {
			properties.put(PROP_PasswordMin, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordCharLogin) == null) {
			properties.put(PROP_PasswordCharLogin, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMax) == null) {
			properties.put(PROP_PasswordMax, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinDigit) == null) {
			properties.put(PROP_PasswordMinDigit, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinAlpha) == null) {
			properties.put(PROP_PasswordMinAlpha, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinChar) == null) {
			properties.put(PROP_PasswordMinChar, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinNonAlpha) == null) {
			properties.put(PROP_PasswordMinNonAlpha, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordDistLogin) == null) {
			properties.put(PROP_PasswordDistLogin, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordDistOld) == null) {
			properties.put(PROP_PasswordDistOld, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordCharOld) == null) {
			properties.put(PROP_PasswordCharOld, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinLowerCase) == null) {
			properties.put(PROP_PasswordMinLowerCase, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(PROP_PasswordMinUpperCase) == null) {
			properties.put(PROP_PasswordMinUpperCase, "0"); //$NON-NLS-1$
			result = true;
		}
		if (properties.get(KEY_LOCKCOUNT_MAX) == null) {
			properties.put(KEY_LOCKCOUNT_MAX, Integer.toString(LOCKCOUNT_MAX));
			result = true;
		}
		if (properties.get(PROP_PWDDURATION) == null) {
			properties.put(PROP_PWDDURATION, Integer.toString(PWD_DURATION));
			result = true;
		}
		if (properties.get(PROP_CASESENSITIVE) == null) {
			properties.put(PROP_CASESENSITIVE, Boolean.toString(DEFAULT_CASESENSITIVE));
			result = true;
		}
		return result;
	}

	@Override
	public void updatedConfiguration(Dictionary<String,Object> properties) {
		if (properties == null) {
			return;
		}
		if (tester != null) {
			tester.setPasswordCharLogin(parseIntegerParameter(properties.get(PROP_PasswordCharLogin), 0));
			tester.setPasswordCharOld(parseIntegerParameter(properties.get(PROP_PasswordCharOld), 0));
			tester.setPasswordDistLogin(parseIntegerParameter(properties.get(PROP_PasswordDistLogin), 0));
			tester.setPasswordDistOld(parseIntegerParameter(properties.get(PROP_PasswordDistOld), 0));
			tester.setPasswordLowerCase(parseIntegerParameter(properties.get(PROP_PasswordMinLowerCase), 0));
			tester.setPasswordMax(parseIntegerParameter(properties.get(PROP_PasswordMax), 0));
			tester.setPasswordMin(parseIntegerParameter(properties.get(PROP_PasswordMin), 0));
			tester.setPasswordMinAlpha(parseIntegerParameter(properties.get(PROP_PasswordMinAlpha), 0));
			tester.setPasswordMinChar(parseIntegerParameter(properties.get(PROP_PasswordMinChar), 0));
			tester.setPasswordMinDigit(parseIntegerParameter(properties.get(PROP_PasswordMinDigit), 0));
			tester.setPasswordMinNonAlpha(parseIntegerParameter(properties.get(PROP_PasswordMinNonAlpha), 0));
			tester.setPasswordUpperCase(parseIntegerParameter(properties.get(PROP_PasswordMinUpperCase), 0));
			tester.setPasswordRepeated(parseIntegerParameter(properties.get(PROP_PasswordRepeated), 0));
			tester.setBlacklist(parseStringParameter(properties.get(PROP_BLACKLIST), "./configuration/blacklist.txt")); //$NON-NLS-1$
		}
		if (properties.get(KEY_LOCKCOUNT_MAX) != null) {
			maxLockCount = Integer.valueOf(properties.get(KEY_LOCKCOUNT_MAX).toString());
			if (maxLockCount <= 0) {
				maxLockCount = 1;
			}
		}
		if (properties.get(PROP_PWDDURATION) != null) {
			pwdDuration = Integer.valueOf(properties.get(PROP_PWDDURATION).toString());
		}
		if (properties.get(PROP_CASESENSITIVE) != null) {
			casesensitive = Boolean.valueOf(properties.get(PROP_CASESENSITIVE).toString());
		}
		Object o = properties.get(PROP_ADMINACCOUNT);
		adminUIDs = new ArrayList<Integer>();
		if (o != null) {
			String s = o.toString().trim();
			final ArrayList<Integer> uids = new ArrayList<Integer>(1);
			for (String id : s.split(" ")) { //$NON-NLS-1$
				try {
					uids.add(Integer.decode(id));
				} catch (NumberFormatException e) {}
			}
			if (uids.size() > 0) {
				adminUIDs = uids;
				int delay = parseIntegerParameter(properties.get(PROP_ADMINLOCK), 0);
				if (delay > 0) {
					unlockDelay = delay * 60000l;
				}
				// Try to unlock all admin account right now...
				try {
					int i = 100;
					for (Integer uid : adminUIDs) {
						recordUserUnlockProcess(uid, 12000 + i);
						i += 100;
					}
				} catch (Exception e) {
					info(e);
				}
			}
		}
	}

	public String getHelp() {
		return Messages.Activator_Console_Command_Help + Messages.Activator_Console_Command_Help_Unlockuser;
	}

	public void _unlockuser(CommandInterpreter ci) throws Exception {
		MetaDataEntity entity = MetaDataEntity.loadEntity(LOCALAUTH);
		if ((entity == null) || (entity.getMapper() == null)) {
			ci.println(Messages.Activator_Error_Nodatasource);
			return;
		}
		String option = ci.nextArgument();
		if (option == null) {
			ci.println(Messages.Activator_Console_Command_list);
			BeanMapList list = entity.dataSelection(); 
			if ((list == null) || (list.size() == 0)) {
				ci.println(Messages.Activator_Console_Command_nolock);
			} else {
				for (BeanMap map: list) {
					StringBuilder sb = new StringBuilder(" ");  //$NON-NLS-1$
					if (map.get(LOCALAUTH_LOGIN) != null) {
						sb.append(map.get(LOCALAUTH_LOGIN));
					}
					if (map.get(LOCALAUTH_USERID) != null) {
						sb.append(" ["); //$NON-NLS-1$
						sb.append(map.getId());
						sb.append("]"); //$NON-NLS-1$
					}
					if (map.get(LOCALAUTH_LOCKED) != null) {
						if (map.getInt(LOCALAUTH_LOCKED) >= maxLockCount) {
							sb.append(Messages.Activator_locked);
						}
						sb.append(" ("); //$NON-NLS-1$
						sb.append(map.get(LOCALAUTH_LOCKED));
						sb.append(Messages.Activator_unsuccessful_tries);
					}
					sb.append(Messages.Activator_Console_Command_limit_date);
					if (map.get(LOCALAUTH_PWDUPDATE) != null) {
						sb.append(map.get(LOCALAUTH_PWDUPDATE));
					} else {
						sb.append(Messages.Activator_Console_Command_unknown);
					}
					ci.println(sb.toString());
				}
			}
		} else {
			int i = 0;
			try {
				i = Integer.parseInt(option);
			} catch (NumberFormatException e) {}
			if (i > 0) {
				entity.dataUpdate(i, //
						AbstractMapperService.list(entity.getAttribute(LOCALAUTH_LOCKED),entity.getAttribute(LOCALAUTH_PWDUPDATE)), //
						AbstractMapperService.list((Object)new Integer(0),(Object)getPwdNextLimitDate()));
				ci.println(Messages.Activator_User_Id+i+Messages.Activator_unlocked);
			} else {
				BeanMapList list = entity.dataSelection(LOCALAUTH_LOCKED, false, LOCALAUTH_LOGIN, option);
				if (list != null) {
					for (BeanMap map : list) {
						entity.dataUpdate(map.getId(), //
								AbstractMapperService.list(entity.getAttribute(LOCALAUTH_LOCKED), entity.getAttribute(LOCALAUTH_PWDUPDATE)), //
								AbstractMapperService.list((Object) new Integer(0), (Object) getPwdNextLimitDate()));
					}
				}
				ci.println(Messages.Activator_User_Login+option+Messages.Activator_unlocked);
			}
			initializeAuthCache();
		}
	}

	/**
	 * (This command will be removed when compatibility with old database will be broken)
	 * 
	 * Update old password (not crypted in database...)
	 * 
	 * @param ci
	 * @throws Exception
	 */
	public void _patchpwd(CommandInterpreter ci) throws Exception {
		MetaDataEntity entity = MetaDataEntity.loadEntity(LOCALAUTH);
		if ((entity == null) || (entity.getMapper() == null)) {
			ci.println(Messages.Activator_Error_Nodatasource);
			return;
		}
		BeanMapList list = entity.getMapper().selection(entity,LOCALAUTH_PASSWORD);
		if (list == null) {
			ci.println(Messages.Activator_Empty_DB);
			return;
		}
		int c = 0;
		MetaDataAttribute attribute = entity.getAttribute(LOCALAUTH_PASSWORD);
		for (BeanMap map: list) {
			String p = map.getString(LOCALAUTH_PASSWORD);
			if ((p != null) && (p.length() < 128)) { // 128 was the length of the Whirlpool hash in hex String.
				char[] pwd = map.getCharArray(LOCALAUTH_PASSWORD);
				if (pwd != null) {
					try {
						entity.getMapper().update(map.getId(), attribute, Crypto.hash(pwd));
					} finally {
						Crypto.clear(pwd);
					}
				}
				if ((c++) % 10 == 0) {
					ci.print('.');
				}
			}
		}
		ci.println();
		ci.println(c + Messages.Activator_Users_updated);
		initializeAuthCache();
	}

	protected void initializeAuthCache() {
		MetaDataEntity entity = MetaDataEntity.loadEntity(LOCALAUTH);
		if ((entity == null) || (entity.getMapper() == null)) {
			return;
		}
		try {
			BeanMapList list = entity.getMapper().selection(entity, LOCALAUTH_ALLATTRIBUTES, false, new NotCriteria(new DeletedCriteria(LOCALAUTH_USERID)), false, null, null, 0, -1);
			if (list != null) {
				authCacheId = new HashMap<Integer, BeanMap>();
				authCacheLogin = new HashMap<String, BeanMap>();
				String s;
				for(BeanMap bm: list) {
					int uid = bm.getInt(LOCALAUTH_USERID);
					if (uid > 0) {
						authCacheId.put(uid, bm);
						s = (String) bm.get(LOCALAUTH_LOGIN);
						if (s != null) {
							authCacheLogin.put(s, bm);
						}
					}
				}
			}
		} catch (NullPointerException e) {
			// This may append if the datasource is not yet configured...
			debug(e);
		}
	}

	public void purgeConnectionCache(int userId) {
		IConnectionCache connectionCache = (IConnectionCache) connectionCacheTracker.getService();
		if (connectionCache != null) {
			connectionCache.purge("user", userId); //$NON-NLS-1$
		}
	}

	public void purgeConnectionCache() {
		IConnectionCache connectionCache = (IConnectionCache) connectionCacheTracker.getService();
		if (connectionCache != null) {
			connectionCache.purgeAll();
		}
	}

	protected void cacheClear() {
		authCacheId = null;
		authCacheLogin = null;
	}
	
	public Date getPwdNextLimitDate() {
		if (pwdDuration <= 0) {
			return new Date(3153600000000l); // environ en 2070...
		}
		GregorianCalendar gc = new GregorianCalendar();
		gc.add(GregorianCalendar.DATE, pwdDuration);
		return gc.getTime();
	}
	
	public PasswordComplexityTester getTester() {
		return tester;
	}
	
	public int getMaxLockCount() {
		return maxLockCount;
	}

	public BeanMap getAuth(int id) {
		if (authCacheId == null) {
			initializeAuthCache();
			if (authCacheId == null) {
				return null;
			}
		}
		return authCacheId.get(id);
	}
	
	public BeanMap getAuth(String login) {
		if (authCacheLogin == null) {
			initializeAuthCache();
			if (authCacheLogin == null) {
				return null;
			}
		}
		return authCacheLogin.get(login);
	}
	
	public void setAuth(BeanMap auth, String oldlogin, int oldUid) {
		if (authCacheId == null) {
			initializeAuthCache();
			if (authCacheId == null) {
				return;
			}
		}
		if (oldUid > 0) {
			authCacheId.remove(oldUid);
		}
		if (auth != null) {
			oldUid = auth.getInt(LOCALAUTH_USERID);
			if (oldUid > 0) {
				authCacheId.put(oldUid, auth);
			}
		}
		if (oldlogin != null) {
			authCacheLogin.remove(oldlogin);
		}
		if (auth != null) {
			oldlogin = (String)auth.get(LOCALAUTH_LOGIN);
			if (oldlogin != null) {
				authCacheLogin.put(oldlogin, auth);
			}
		}
	}

	public boolean isCaseSensitive() {
		return casesensitive;
	}
	
	public String getMessage(String key, Language language) {
		return messages.get(key, language);
	}
	
	public void recordUserLock(int uid) {
		if ((unlockDelay > 0) && (adminUIDs != null) && adminUIDs.contains(uid)) {
			recordUserUnlockProcess(uid, unlockDelay);
		}
	}
	
	private void recordUserUnlockProcess(final int uid, long delay) {
		final Timer t = new Timer("User Unlock management"); //$NON-NLS-1$
		unlockTimers.put(uid, t);
		t.schedule(new TimerTask() {
			@Override
			public void run() {
				unlockUser(uid, false);
			}
		}, delay);
	}
	
	private void unlockUser(final int uid, final boolean secondTry) {
		Timer t = unlockTimers.remove(uid);
		t.cancel();
		try {
			MetaDataEntity entity = MetaDataEntity.loadEntity(LOCALAUTH);
			if ((entity != null) && (entity.getMapper() != null)) {
				entity.dataUpdate(AbstractMapperService.list(entity.getAttribute(LOCALAUTH_LOCKED)), //
						AbstractMapperService.list((Object) new Integer(0)), //
						new EqualCriteria(LOCALAUTH_USERID, uid));
				return;
			}
		} catch (Exception e) {
			if (secondTry) {
				error("Unrecoverable error wuring local user (" + uid + ") unclock: " + e.getLocalizedMessage(), e);
				return;
			}
			info("There was an error while trying to unlock a local User (another try will be run: " + e.getLocalizedMessage());
			// if there is an error, we assume that this error is recoverable in a next attempt...
		}
		t = new Timer("User Unlock management (retry)"); //$NON-NLS-1$
		unlockTimers.put(uid, t);
		t.schedule(new TimerTask() {
			@Override
			public void run() {
				unlockUser(uid, true);
			}
		}, 15000);
	}
}
