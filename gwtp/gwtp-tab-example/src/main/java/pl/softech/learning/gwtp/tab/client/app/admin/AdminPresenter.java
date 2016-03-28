package pl.softech.learning.gwtp.tab.client.app.admin;

import pl.softech.learning.gwtp.tab.client.app.ApplicationPresenter;
import pl.softech.learning.gwtp.tab.client.place.NameTokens;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.TabInfo;
import com.gwtplatform.mvp.client.proxy.TabContentProxyPlace;

/**
 * @author sledzs
 * 
 */
public class AdminPresenter extends Presenter<AdminPresenter.MyView, AdminPresenter.MyProxy> {
    
    @ProxyCodeSplit
    @NameToken(NameTokens.admin)
    @TabInfo(container = ApplicationPresenter.class, label = "Admin", priority = 2)
    public interface MyProxy extends TabContentProxyPlace<AdminPresenter> {
    }

    public interface MyView extends View {
    }

    @Inject
    AdminPresenter(EventBus eventBus, MyView view, MyProxy proxy) {
	super(eventBus, view, proxy, ApplicationPresenter.TYPE_SetTabContent);
    }
}
