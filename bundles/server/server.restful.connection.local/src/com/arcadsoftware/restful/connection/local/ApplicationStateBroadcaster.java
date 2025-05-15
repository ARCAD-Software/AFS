package com.arcadsoftware.restful.connection.local;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;

import org.restlet.data.Language;

import com.arcadsoftware.rest.connection.ApplicationStatePlot;
import com.arcadsoftware.rest.connection.IApplicationStateBroadcaster;
import com.arcadsoftware.rest.connection.IConnectionCredential;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.ApplicationStatePlot.PlotLevel;

public class ApplicationStateBroadcaster implements IApplicationStateBroadcaster {

	public ApplicationStateBroadcaster(Activator activator) {
		super();
	}
	
	@Override
	public ApplicationStatePlot broadcast(IConnectionUserBean user, Language language) {
		List<IConnectionCredential> creds = user.getCredential(LocalConnectionCredential.class);
		if ((creds == null) || creds.isEmpty()) {
			return null;
		}
		final LocalConnectionCredential cred = (LocalConnectionCredential) creds.get(0);
		if (cred == null) {
			return null;
		}
		final Instant now = Instant.now();
		final Instant limit = cred.getLimit().toInstant();
		if (limit.isAfter(now)) {
			if (limit.isBefore(now.plus(6, ChronoUnit.HOURS))) {
				return new ApplicationStatePlot(Activator.LOCALAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimitsixhours", language), 360);
			}
			if (limit.isBefore(now.plus(2, ChronoUnit.DAYS))) {
				return new ApplicationStatePlot(Activator.LOCALAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimittwodays", language), 2520);
			}
			if (limit.isBefore(now.plus(7, ChronoUnit.DAYS))) {
				return new ApplicationStatePlot(Activator.LOCALAUTH, PlotLevel.CRITICAL, Activator.getMessage("passwordlimitoneweek", language), 7200);
			}
		}
		return null;
	}

}
