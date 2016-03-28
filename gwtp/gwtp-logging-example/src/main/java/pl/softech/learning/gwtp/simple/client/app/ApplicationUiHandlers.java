package pl.softech.learning.gwtp.simple.client.app;

import java.util.logging.Level;

import com.gwtplatform.mvp.client.UiHandlers;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public interface ApplicationUiHandlers extends UiHandlers {
	void log(String msg);

	void throwNPException();

	void throwCallStackSizeExceededException();

	void setCurrentLogginLevel(Level level);
}
