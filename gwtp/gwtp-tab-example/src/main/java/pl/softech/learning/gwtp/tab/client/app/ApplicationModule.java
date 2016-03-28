package pl.softech.learning.gwtp.tab.client.app;

import pl.softech.learning.gwtp.tab.client.app.admin.AdminPresenter;
import pl.softech.learning.gwtp.tab.client.app.admin.AdminView;
import pl.softech.learning.gwtp.tab.client.app.home.HomeTabPresenter;
import pl.softech.learning.gwtp.tab.client.app.home.HomeTabView;
import pl.softech.learning.gwtp.tab.client.app.home.info.InfoPresenter;
import pl.softech.learning.gwtp.tab.client.app.home.info.InfoView;
import pl.softech.learning.gwtp.tab.client.app.home.main.HomePresenter;
import pl.softech.learning.gwtp.tab.client.app.home.main.HomeView;
import pl.softech.learning.gwtp.tab.client.app.ui.UIModule;

import com.gwtplatform.mvp.client.gin.AbstractPresenterModule;

public class ApplicationModule extends AbstractPresenterModule {
    @Override
    protected void configure() {
	
	install(new UIModule());
	
	bindPresenter(ApplicationPresenter.class, ApplicationPresenter.MyView.class, ApplicationView.class,
		ApplicationPresenter.MyProxy.class);
	
	bindPresenter(HomeTabPresenter.class, HomeTabPresenter.MyView.class, HomeTabView.class,
		HomeTabPresenter.MyProxy.class);
	
	bindPresenter(AdminPresenter.class, AdminPresenter.MyView.class, AdminView.class,
		AdminPresenter.MyProxy.class);
	
	bindPresenter(InfoPresenter.class, InfoPresenter.MyView.class, InfoView.class,
		InfoPresenter.MyProxy.class);
	
	bindPresenter(HomePresenter.class, HomePresenter.MyView.class, HomeView.class,
		HomePresenter.MyProxy.class);
    }
}
