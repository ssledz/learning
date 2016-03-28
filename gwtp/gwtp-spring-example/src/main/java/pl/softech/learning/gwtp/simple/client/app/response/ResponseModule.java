package pl.softech.learning.gwtp.simple.client.app.response;

import pl.softech.learning.gwtp.simple.client.app.response.ResponsePresenter;
import pl.softech.learning.gwtp.simple.client.app.response.ResponseView;

import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;

public class ResponseModule extends AbstractPresenterModule {
    @Override
    protected void configure() {
	bindPresenter(ResponsePresenter.class, ResponsePresenter.MyView.class, ResponseView.class,
		ResponsePresenter.MyProxy.class);
    }
}
