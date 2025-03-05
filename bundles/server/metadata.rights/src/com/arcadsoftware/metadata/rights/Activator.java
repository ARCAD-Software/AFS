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
package com.arcadsoftware.metadata.rights;

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.List;

import javax.sql.DataSource;

import org.apache.felix.service.command.CommandProcessor;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.ServiceReference;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.restlet.data.Language;

import com.arcadsoftware.beanmap.BeanMap;
import com.arcadsoftware.beanmap.BeanMapList;
import com.arcadsoftware.beanmap.xml.XmlBeanMapStream;
import com.arcadsoftware.metadata.IMapperService;
import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.metadata.ReferenceLine;
import com.arcadsoftware.metadata.sql.MapperSQLService;
import com.arcadsoftware.osgi.AbstractActivator;
import com.arcadsoftware.rest.connection.ConnectionUserBean;
import com.arcadsoftware.rest.connection.IConnectionInfoService;
import com.arcadsoftware.rest.connection.IProfileRightsListService;
import com.arcadsoftware.rest.connection.Profile;
import com.arcadsoftware.rest.connection.Right;
import com.arcadsoftware.rest.connection.RightInfo;

/*
 * Cet activateur gère :
 * - la déclaration des Rights par fichiers xml.
 * - Le chargement des informations de connexion lié à l'entité user.
 */
public class Activator extends AbstractActivator implements BundleListener, IConnectionInfoService, IProfileRightsListService {

	private static final String USER = "user"; //$NON-NLS-1$
	private static final String USER_TITLE = "title.name"; //$NON-NLS-1$
	private static final String USER_FIRSTNAME = "firstname"; //$NON-NLS-1$
	private static final String USER_LASTNAME = "lastname"; //$NON-NLS-1$
	private static final String USER_PRINCIPAL = "principal"; //$NON-NLS-1$
	private static final String USER_CLIENT = "client"; //$NON-NLS-1$
	private static final String USER_ATTRIBUTES = USER_TITLE + ' ' + USER_FIRSTNAME + ' ' + USER_LASTNAME + ' ' + USER_CLIENT + ' ' + USER_PRINCIPAL;
	private static final String USERRIGHT = "userRight"; //$NON-NLS-1$
	private static final String DEFAULTRIGHTSFILENAME = "/META-INF/rights.xml"; //$NON-NLS-1$
	private static final String RIGHTSHEADER = "Arcad-Rights"; //$NON-NLS-1$
	private static final String EQUINOX_COMMON = "org.eclipse.equinox.common"; //$NON-NLS-1$
	protected static final String RIGHT = "right"; //$NON-NLS-1$
	protected static final String RIGHTCATEGORY = "list/rightcategory"; //$NON-NLS-1$
	protected static final String RIGHT_CODE = "code"; //$NON-NLS-1$
	protected static final String RIGHT_NAME = "name"; //$NON-NLS-1$
	protected static final String RIGHT_CATEGORY = "category"; //$NON-NLS-1$
	private static final String PARAM = "param"; //$NON-NLS-1$
	private static final String RIGHTANDPARAM = RIGHT + ' ' + PARAM;

	private final HashMap<Integer, BeanMap> rights = new HashMap<Integer, BeanMap>();;
	private final HashMap<Integer, BeanMap> rightscategories = new HashMap<Integer, BeanMap>();
	private final XmlBeanMapStream xs = new XmlBeanMapStream();
	private volatile ReferenceLine rightName;

	@Override
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		Dictionary<String, Object> props = RightsMapperService.mapperProperties("mem:rights", false, true, false, false, false); //$NON-NLS-1$
		props.put(CommandProcessor.COMMAND_SCOPE, "arcad"); //$NON-NLS-1$
		props.put(CommandProcessor.COMMAND_FUNCTION, new String[] {"rights"}); //$NON-NLS-1$
		registerService(RightsMapperService.clazz, new RightsMapperService(this), props);
		registerService(IConnectionInfoService.clazz, this);
		registerService(IProfileRightsListService.class, this);
		if (isCommonStarted()) {
			proceedDelayedBundleScan();
		}
		bundleContext.addBundleListener(this);
	}

	private void proceedDelayedBundleScan() {
		for (Bundle bundle:getContext().getBundles()) {
			if (bundle.getState() == Bundle.ACTIVE) {
				addBundle(bundle);
			}
		}
	}

	private boolean isCommonStarted() {
		for (Bundle b:getContext().getBundles()) {
			if (EQUINOX_COMMON.equals(b.getSymbolicName()) && (b.getState() == Bundle.ACTIVE)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		bundleContext.removeBundleListener(this);
	}

	public void bundleChanged(BundleEvent event) {
		if (event.getType() == BundleEvent.STARTED) {
			if (EQUINOX_COMMON.equals(event.getBundle().getSymbolicName())) {
				proceedDelayedBundleScan();
			} else if (isCommonStarted()) {
				addBundle(event.getBundle());
			}
		}
	}

	public void addBundle(Bundle bundle) {
		File file = getRightsFile(bundle);
		if ((file != null) && file.isFile() &&
				// Suppression de tous les bundles non arcad !
				(bundle.getSymbolicName().startsWith("com.arcadsoftware.") || //$NON-NLS-1$
				 bundle.getSymbolicName().startsWith("com.dropssoftware."))) { //$NON-NLS-1$
			BeanMapList newRights = new BeanMapList();
			try (FileInputStream fis = new FileInputStream(file)) {
				Object o = xs.fromXML(fis);
				if (o instanceof BeanMap) {
					if (addRight((BeanMap) o)) {
						newRights.add((BeanMap) o);
					}
				} else if (o instanceof BeanMapList) {
					for (BeanMap bm: (BeanMapList) o) {
						if (addRight(bm)) {
							newRights.add(bm);
						}
					}
				} else {
					warn("Invalid or empty rights.xml file: " + file.getAbsolutePath());
				}
			} catch (Exception e) {
				error("Error during Rights loading process", e); //$NON-NLS-1$
			}
			if (!newRights.isEmpty()) {
				sendNewRightsEvent(newRights, bundle.getSymbolicName());
			}
		}
	}

	private void sendNewRightsEvent(BeanMapList newRights, String source) {
		ServiceReference<EventAdmin> sr = getContext().getServiceReference(EventAdmin.class);
		if (sr != null) {
			EventAdmin ea = getContext().getService(sr);
			if (ea != null) {
				HashMap<String, Object> props = new HashMap<String, Object>();
				props.put("rights", newRights); //$NON-NLS-1$
				props.put("source", source); //$NON-NLS-1$
				ea.postEvent(new Event("com/arcadsoftware/metadata/right/add", props)); //$NON-NLS-1$
			}
		}
	}

	protected File getRightsFile(Bundle bundle) {
		String filename = DEFAULTRIGHTSFILENAME;
		Dictionary<?, ?> headers = bundle.getHeaders();
		if (headers != null) {
			Object o = headers.get(RIGHTSHEADER);
			if ((o != null) && (o.toString().length() > 0)) {
				filename = o.toString();
			}
		}
		// Do not use bundle classpath.
		URL url = bundle.getEntry(filename);
		if (url == null) {
			// Use bundle classpath.
			url = bundle.getResource(filename);
			if (url == null) {
				return null;
			}
		}
		return toFile(url);
	}

	/**
	 * Return true only if the right database need to be update (ignore already loaded rights and categories).
	 * 
	 * @param bean
	 * @return
	 */
	private boolean addRight(BeanMap bean) {
		if (RIGHT.equals(bean.getType())) {
			int n = bean.getId();
			if (n > 0) {
				synchronized (rights) {
					BeanMap b = rights.get(n);
					if (b != null) {
						// Si on a déjà chargé ce droit alors on l'ignore (le premier chargé prime,
						// puisque'on ne peut pas connaitre l'ordre de chargement).
						if (!b.getString(RIGHT_CODE, "").equals(bean.getString(RIGHT_CODE))) { //$NON-NLS-1$
							// Si les codes sont différent alors il peut y avoir une erreur !
							warn(String.format("Duplicated Right declaration (#%d), \"%s\" <> \"%s\".", n, bean.getString(RIGHT_CODE), b.getString(RIGHT_CODE))); //$NON-NLS-1$
						}
						return false;
					}
					rights.put(n, bean);
				}
				// on affecte le pseudo attribut "category." de la valeur courante de la catégorie si elle
				// a déjà été chargée.
				synchronized (rightscategories) {
					bean.addAll(RIGHT_CATEGORY + '.', rightscategories.get(bean.get(RIGHT_CATEGORY)));
				}
				return true;
			}
		} else if ("rightcategory".equalsIgnoreCase(bean.getType()) || RIGHTCATEGORY.equals(bean.getType())) { //$NON-NLS-1$
			bean.setType(RIGHTCATEGORY);
			synchronized (rightscategories) {
				BeanMap b = rightscategories.get(bean.getId());
				if ((b != null) && !b.getString(RIGHT_CODE, "").equals(bean.getString(RIGHT_CODE))) { //$NON-NLS-1$
					warn(String.format("Duplicated list/rightcategory declaration (#%d), \"%s\" <> \"%s\".", bean.getId(), bean.getString(RIGHT_CODE), b.getString(RIGHT_CODE))); //$NON-NLS-1$
				}
				rightscategories.put(bean.getId(), bean);
			}
			// On met à jour les pseudo attributs "category." pour tous les droits consernés, déjà chargés.
			synchronized (rights) {
				for(BeanMap right: rights.values()) {
					if (right.getInt(RIGHT_CATEGORY) == bean.getId()) {
						right.addAll(RIGHT_CATEGORY + '.', bean);
					}
				}
			}
		}
		return false;
	}

	public Collection<BeanMap> getRights() {
		synchronized (rights) {
			return new ArrayList<BeanMap>(rights.values());
		}
	}
	
	public BeanMap getRightBean(int number) {
		synchronized (rights) {
			return rights.get(number);
		}
	}

	public BeanMap getRightCategoryBean(int itemId) {
		synchronized (rightscategories) {
			return rightscategories.get(itemId);
		}
	}

	public Collection<BeanMap> getRightCategories() {
		synchronized (rightscategories) {
			return new ArrayList<BeanMap>(rightscategories.values());
		}
	}

	public ConnectionUserBean loadUser(String userType, int id) {
		if (!USER.equals(userType)) {
			return null;
		}
		final MetaDataEntity entity = MetaDataEntity.loadEntity(userType);
		if (entity == null) {
			return null;
		}		// Load the User information from database: 
		BeanMap user = entity.dataSelection(id, USER_ATTRIBUTES, false);
		if (user == null) {
			return null;
		}
		final ConnectionUserBean result = new ConnectionUserBean(userType, id);
		// Set client reference
		int pid = user.getInt(USER_PRINCIPAL);
		if (pid <= 0) {
			pid = user.getInt(USER_CLIENT);
		}
		result.setPrincipal(pid);
		// Define user full name
		StringBuilder fn = new StringBuilder();
		String s = user.getString(USER_TITLE);
		if (s != null) {
			fn.append(s);
		}
		s = user.getString(USER_FIRSTNAME);
		if (s != null) {
			if (fn.length() > 0) {
				fn.append(' ');
			}
			fn.append(s);
		}
		s = user.getString(USER_LASTNAME);
		if (s != null) {
			if (fn.length() > 0) {
				fn.append(' ');
			}
			fn.append(s);
		}
		result.setFullname(fn.toString());
		Profile profile = new Profile();
		result.setProfile(profile);
		// Load user rights.
		MetaDataEntity rightentity = MetaDataEntity.loadEntity(USERRIGHT);
		if (rightentity == null) {
			// Performs a reverse-link with subdivision selection, with optimized SQL:
			IMapperService userMapper = entity.getMapper();
			if (userMapper instanceof MapperSQLService) {
				DataSource ds = ((MapperSQLService) userMapper).getDataSource();
				try (Connection cn = ds.getConnection()) {
					try (PreparedStatement ps = cn.prepareStatement("with recursive x(r) as (select UPF_PRF_ID from USER_PROFILES where UPF_USR_ID = ? union all select PRP_CHILD from SUBPROFILES inner join x on (x.r = PRP_PARENT)) select distinct PFR_RIGHT, PFR_PARAMETER from x left outer join PROFILE_RIGHTS on (PFR_PRF_ID = x.r) where PFR_DELETED = 0")) {
						ps.setInt(1, id);
						try (ResultSet rs = ps.executeQuery()) {
							while (rs.next()) {
								profile.addRight(new Right(rs.getInt(1), rs.getInt(2)));
							}
						}
					}
				} catch (SQLException e) {
					debug("Unable to get user rigths with AFS eneities: " + e.getLocalizedMessage());
				}
			}
		} else {
			// if there is a Entity providing a direct link between Users and Rights, we use it.
			for (BeanMap bean: rightentity.dataSelection(RIGHTANDPARAM, false, USER, id)) {
				profile.addRight(new Right(bean.getInt(RIGHT), bean.getInt(PARAM)));
			}
		}
		return result;
	}

	@Override
	public List<RightInfo> getProfileRights(Profile profile, Language language) {
		ArrayList<RightInfo> result = new ArrayList<>();
		if (rightName == null) {
			MetaDataEntity entity = MetaDataEntity.loadEntity(RIGHT);
			if (entity == null) {
				return result;
			}
			rightName = new ReferenceLine(entity.getAttribute(RIGHT_NAME));
		}
		for (BeanMap r: rights.values()) {
			Collection<Right> pr = profile.getParams(r.getId());
			if ((pr == null) || pr.isEmpty()) {
				if (profile.hasRight(r.getId())) {
					String code = r.getString(RIGHT_CODE);
					if (code == null) {
						code = Integer.toString(r.getId());
					}
					result.add(new RightInfo(r.getId(), 0, code, rightName.translate(code, language)));
				}
			} else {
				String code = r.getString(RIGHT_CODE);
				if (code == null) {
					code = Integer.toString(r.getId());
				}
				String name = rightName.translate(code, language);
				for (Right right: pr) {
					result.add(new RightInfo(r.getId(), right.getParam(), code, name));
				}
			}
		}
		return result;
	}
}