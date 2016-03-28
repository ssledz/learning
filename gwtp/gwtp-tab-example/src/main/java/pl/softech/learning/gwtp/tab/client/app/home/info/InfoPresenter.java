package pl.softech.learning.gwtp.tab.client.app.home.info;

import pl.softech.learning.gwtp.tab.client.app.home.HomeTabPresenter;
import pl.softech.learning.gwtp.tab.client.place.NameTokens;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.proxy.ProxyPlace;
import com.gwtplatform.mvp.client.proxy.RevealContentEvent;

/**
 * @author sledzs
 * 
 */
public class InfoPresenter extends Presenter<InfoPresenter.MyView, InfoPresenter.MyProxy> {
    
    @ProxyCodeSplit
    @NameToken(NameTokens.homeInfo)
    public interface MyProxy extends ProxyPlace<InfoPresenter> {
    }

    public interface MyView extends View {
    }

    @Inject
    InfoPresenter(EventBus eventBus, MyView view, MyProxy proxy) {
	super(eventBus, view, proxy);
    }
    
    @Override
    protected void revealInParent() {
	RevealContentEvent.fire(this, HomeTabPresenter.TYPE_SetContent, this);
    }
}
