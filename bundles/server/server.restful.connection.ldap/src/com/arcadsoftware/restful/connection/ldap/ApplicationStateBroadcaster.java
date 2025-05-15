package com.arcadsoftware.restful.connection.ldap;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;

import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.ApplicationStatePlot;
import com.arcadsoftware.rest.connection.IApplicationStateBroadcaster;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.ApplicationStatePlot.PlotLevel;

public class ApplicationStateBroadcaster  implements IApplicationStateBroadcaster {

	public ApplicationStateBroadcaster(Activator activator) {
		super();
	}
	
	@Override
	public ApplicationStatePlot broadcast(IConnectionUserBean user, Language language) {
		List<IConnectionCredential> creds = user.getCredential(LdapConnectionCredential.class);
		if ((creds == null) || creds.isEmpty()) {
			return null;
		}
		final LdapConnectionCredential cred = (LdapConnectionCredential) creds.get(0);
		if (cred == null) {
			return null;
		}
		final Date limitation = cred.getLimitation();
		if (limitation != null) {
			final Instant now = Instant.now();
			final Instant limit = limitation.toInstant();
			if (limit.isAfter(now)) {
				if (limit.isBefore(now.plus(2, ChronoUnit.HOURS))) {
					return new ApplicationStatePlot(Activator.LDAPAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimittwohours", language), 360);
				}
				if (limit.isBefore(now.plus(1, ChronoUnit.DAYS))) {
					return new ApplicationStatePlot(Activator.LDAPAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimittomorow", language), 2520);
				}
				if (limit.isBefore(now.plus(7, ChronoUnit.DAYS))) {
					return new ApplicationStatePlot(Activator.LDAPAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimitoneweek", language), 7200);
				}
			}
		}
		return null;
	}


}
