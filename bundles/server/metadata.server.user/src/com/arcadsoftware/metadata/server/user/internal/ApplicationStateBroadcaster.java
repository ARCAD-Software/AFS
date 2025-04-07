package com.arcadsoftware.metadata.server.user.internal;

import org.restlet.data.Language;

import com.arcadsoftware.metadata.MetaDataEntity;
import com.arcadsoftware.rest.connection.ApplicationStatePlot;
import com.arcadsoftware.rest.connection.IApplicationStateBroadcaster;
import com.arcadsoftware.rest.connection.IConnectionUserBean;
import com.arcadsoftware.rest.connection.ApplicationStatePlot.PlotLevel;

public class ApplicationStateBroadcaster implements IApplicationStateBroadcaster {

	private static final String USERLIMIT = "USERLIMIT"; //$NON-NLS-1$
	
	private final Activator activator;
	
	public ApplicationStateBroadcaster(Activator activator) {
		super();
		this.activator = activator;
	}

	@Override
	public ApplicationStatePlot broadcast(IConnectionUserBean user, Language language) {
		int max = activator.getUserMax();
		if (max > 0) {
			MetaDataEntity entity = MetaDataEntity.loadEntity("user");
			if (entity != null) {
				int userCount = entity.dataCount();
				if (userCount >= max) {
					final PlotLevel level;
					if (activator.isUserMaxlock()) {
						// No need to be to alarmist here, any creation of a new user will be blocked...
						level = PlotLevel.WARN;
					} else if (userCount > max) {
						level = PlotLevel.BLOCKER;
					} else {
						level = PlotLevel.CRITICAL;
					}
					return new ApplicationStatePlot(USERLIMIT, level, Activator.translate("limituserexceeded", language, max), 5);
				}
				if (max > 3) {
					int max10p = max / 10;
					if (max10p <= 0) {
						max10p = 1;
					}
					if (userCount >= (max - max10p)) {
						final PlotLevel level;
						if (activator.isUserMaxlock()) {
							// No need to be to alarmist here, any creation of a new user will be blocked...
							level = PlotLevel.INFO;
						} else {
							level = PlotLevel.WARN;
						}
						return new ApplicationStatePlot(USERLIMIT, level, Activator.translate("limituserexceeding", language, userCount, max), 5);
					}
				}
			}
		}
		return null;
	}

}
