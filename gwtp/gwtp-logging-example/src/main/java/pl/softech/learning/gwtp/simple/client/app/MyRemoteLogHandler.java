package pl.softech.learning.gwtp.simple.client.app;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.gwt.core.client.GWT;
import com.google.gwt.logging.client.RemoteLogHandlerBase;
import com.google.gwt.logging.shared.RemoteLoggingService;
import com.google.gwt.logging.shared.RemoteLoggingServiceAsync;
import com.google.gwt.user.client.Window.Location;
import com.google.gwt.user.client.rpc.AsyncCallback;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class MyRemoteLogHandler extends RemoteLogHandlerBase {

	class DefaultCallback implements AsyncCallback<String> {
		public void onFailure(Throwable caught) {
			wireLogger.log(Level.SEVERE, "Remote logging failed: ", caught);
		}

		public void onSuccess(String result) {
			if (result != null) {
				wireLogger.severe("Remote logging failed: " + result);
			} else {
				wireLogger.finest("Remote logging message acknowledged");
			}
		}
	}

	private AsyncCallback<String> callback;
	private RemoteLoggingServiceAsync service;

	private boolean forceRemoteLogging = false;

	public MyRemoteLogHandler() {
		service = (RemoteLoggingServiceAsync) GWT.create(RemoteLoggingService.class);
		this.callback = new DefaultCallback();
		forceRemoteLogging = parseBoolean(Location.getParameter("forceRemoteLogging"));
	}

	private boolean parseBoolean(String arg) {
		if (arg == null) {
			return false;
		}
		try {
			return Boolean.parseBoolean(arg);
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public boolean isLoggable(LogRecord record) {
		if (!super.isLoggable(record)) {
			return false;
		}

		return forceRemoteLogging || record.getLevel() == Level.SEVERE;
	}

	@Override
	public void publish(LogRecord record) {
		if (isLoggable(record)) {
			service.logOnServer(record, callback);
		}
	}

}
