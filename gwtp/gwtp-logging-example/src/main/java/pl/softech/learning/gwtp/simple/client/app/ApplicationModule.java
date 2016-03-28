package pl.softech.learning.gwtp.simple.client.app;

import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class ApplicationModule extends AbstractPresenterModule {
	@Override
	protected void configure() {
		bindPresenter(ApplicationPresenter.class, ApplicationPresenter.MyView.class, ApplicationView.class,
				ApplicationPresenter.MyProxy.class);

	}
}
