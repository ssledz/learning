package pl.softech.learning.gwtp.tab.client.app.home.main;

import pl.softech.learning.gwtp.tab.client.app.home.HomeTabPresenter;
import pl.softech.learning.gwtp.tab.client.place.NameTokens;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.proxy.ProxyPlace;

/**
 * @author sledzs
 * 
 */
public class HomePresenter extends Presenter<HomePresenter.MyView, HomePresenter.MyProxy> {
    
    @ProxyCodeSplit
    @NameToken(NameTokens.home)
    public interface MyProxy extends ProxyPlace<HomePresenter> {
    }

    public interface MyView extends View {
    }

    @Inject
    HomePresenter(EventBus eventBus, MyView view, MyProxy proxy) {
	super(eventBus, view, proxy, HomeTabPresenter.TYPE_SetContent);
    }
}
