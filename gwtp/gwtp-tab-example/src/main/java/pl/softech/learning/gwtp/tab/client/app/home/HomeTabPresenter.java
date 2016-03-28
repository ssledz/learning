package pl.softech.learning.gwtp.tab.client.app.home;

import pl.softech.learning.gwtp.tab.client.app.ApplicationPresenter;
import pl.softech.learning.gwtp.tab.client.place.NameTokens;

import com.google.gwt.event.shared.GwtEvent.Type;
import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.ContentSlot;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.TabInfo;
import com.gwtplatform.mvp.client.proxy.NonLeafTabContentProxy;
import com.gwtplatform.mvp.client.proxy.RevealContentHandler;

/**
 * @author sledzs
 * 
 */
public class HomeTabPresenter extends Presenter<HomeTabPresenter.MyView, HomeTabPresenter.MyProxy> {

    @ProxyCodeSplit
    @TabInfo(container = ApplicationPresenter.class, label = "Home", priority = 1, nameToken = NameTokens.home)
    public interface MyProxy extends NonLeafTabContentProxy<HomeTabPresenter> {
    }

    @ContentSlot
    public static final Type<RevealContentHandler<?>> TYPE_SetContent = new Type<RevealContentHandler<?>>();

    public interface MyView extends View {
    }

    @Inject
    HomeTabPresenter(EventBus eventBus, MyView view, MyProxy proxy) {
	super(eventBus, view, proxy, ApplicationPresenter.TYPE_SetTabContent);
    }

}
