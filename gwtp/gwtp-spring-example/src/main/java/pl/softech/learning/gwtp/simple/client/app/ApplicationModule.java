package pl.softech.learning.gwtp.simple.client.app;

import pl.softech.learning.gwtp.simple.client.app.response.ResponseModule;

import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;

public class ApplicationModule extends AbstractPresenterModule {
    @Override
    protected void configure() {
	
	install(new ResponseModule());
	
	bindPresenter(ApplicationPresenter.class, ApplicationPresenter.MyView.class, ApplicationView.class,
		ApplicationPresenter.MyProxy.class);
    }
}
