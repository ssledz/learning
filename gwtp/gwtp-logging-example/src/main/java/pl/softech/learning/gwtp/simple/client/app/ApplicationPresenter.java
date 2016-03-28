package pl.softech.learning.gwtp.simple.client.app;

import java.util.logging.Level;
import java.util.logging.Logger;

import pl.softech.learning.gwtp.simple.client.place.NameTokens;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyStandard;
import com.gwtplatform.mvp.client.proxy.ProxyPlace;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class ApplicationPresenter extends Presenter<ApplicationPresenter.MyView, ApplicationPresenter.MyProxy> implements
		ApplicationUiHandlers {

	private Logger logger = Logger.getLogger(ApplicationPresenter.class.getName());

	@ProxyStandard
	@NameToken(NameTokens.home)
	public interface MyProxy extends ProxyPlace<ApplicationPresenter> {
	}

	public interface MyView extends View, HasUiHandlers<ApplicationUiHandlers> {
		void resetAndFocus();

		void setLogger(Logger logger);

		void setLoggingLevel(Level level);
	}

	private Level currentLevel = Level.SEVERE;
	
	@Inject
	ApplicationPresenter(final EventBus eventBus, final MyView view, final MyProxy proxy) {
		super(eventBus, view, proxy, RevealType.Root);

		getView().setUiHandlers(this);
		getView().setLogger(logger);
	}

	@Override
	public void log(final String msg) {
		logger.log(currentLevel, msg);
	}

	@Override
	public void throwNPException() {
		final Integer tmp = null;
		innerMethod(tmp);
	}
	
	@Override
	public void throwCallStackSizeExceededException() {
		internalThrowCallStackSizeExceededException();
	}
	
	private native void  internalThrowCallStackSizeExceededException() /*-{
	  $wnd.a = function() { $wnd.a(); }
	  $wnd.a();
	}-*/;
	
	private void innerMethod(final Integer arg) {
		arg.byteValue();
	}

	@Override
	protected void onReset() {
		super.onReset();

		getView().setLoggingLevel(currentLevel);
		getView().resetAndFocus();
	}

	@Override
	public void setCurrentLogginLevel(Level level) {
		currentLevel = level;
	}

}
