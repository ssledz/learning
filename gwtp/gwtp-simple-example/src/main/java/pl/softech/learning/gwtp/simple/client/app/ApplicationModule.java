package pl.softech.learning.gwtp.simple.client.app;

import pl.softech.learning.gwtp.simple.client.app.response.ResponsePresenter;
import pl.softech.learning.gwtp.simple.client.app.response.ResponseView;

import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;

public class ApplicationModule extends AbstractPresenterModule {
    @Override
    protected void configure() {
	bindPresenter(ApplicationPresenter.class, ApplicationPresenter.MyView.class, ApplicationView.class,
		ApplicationPresenter.MyProxy.class);

	bindPresenter(ResponsePresenter.class, ResponsePresenter.MyView.class, ResponseView.class,
		ResponsePresenter.MyProxy.class);
    }
}
