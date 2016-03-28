package pl.softech.learning.gwtp.simple.client.app;

import pl.softech.learning.gwtp.simple.client.place.NameTokens;
import pl.softech.learning.gwtp.simple.client.place.TokenParameters;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyStandard;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.gwtplatform.mvp.client.proxy.ProxyPlace;

public class ApplicationPresenter extends Presenter<ApplicationPresenter.MyView, ApplicationPresenter.MyProxy>
	implements ApplicationUiHandlers {

    @ProxyStandard
    @NameToken(NameTokens.home)
    public interface MyProxy extends ProxyPlace<ApplicationPresenter> {
    }

    public interface MyView extends View, HasUiHandlers<ApplicationUiHandlers> {
	void resetAndFocus();

	void setError(String errorText);
    }

    private final PlaceManager placeManager;

    @Inject
    ApplicationPresenter(EventBus eventBus, MyView view, MyProxy proxy, PlaceManager placeManager) {
	super(eventBus, view, proxy, RevealType.Root);

	this.placeManager = placeManager;

	getView().setUiHandlers(this);
    }

    @Override
    public void sendName(String name) {
	PlaceRequest responsePlaceRequest = new PlaceRequest.Builder().nameToken(NameTokens.response)
		.with(TokenParameters.NAME, name).build();
	placeManager.revealPlace(responsePlaceRequest);
    }

    @Override
    protected void onReset() {
	super.onReset();

	getView().resetAndFocus();
    }

}
