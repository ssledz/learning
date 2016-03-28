package pl.softech.learning.gwtp.simple.client.app;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.GWT.UncaughtExceptionHandler;
import com.gwtplatform.mvp.client.PreBootstrapper;
import static pl.softech.learning.gwtp.simple.client.app.LogUtil.*;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class PreBootstrapperImpl implements PreBootstrapper {

	private Logger logger = Logger.getLogger("PreBootstrapperImpl");

	@Override
	public void onPreBootstrap() {
		GWT.setUncaughtExceptionHandler(new UncaughtExceptionHandler() {
			@Override
			public void onUncaughtException(final Throwable e) {
				logger.log(Level.SEVERE, "", unwrap(e));
			}
		});
	}
}
