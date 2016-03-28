package pl.softech.learning.gwtp.simple.client.app.response;

import pl.softech.learning.gwtp.simple.client.place.NameTokens;
import pl.softech.learning.gwtp.simple.client.place.TokenParameters;

import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.gwtplatform.mvp.client.proxy.ProxyPlace;

public class ResponsePresenter extends Presenter<ResponsePresenter.MyView, ResponsePresenter.MyProxy> implements
	ResponseUiHandlers {
    
    @ProxyCodeSplit
    @NameToken(NameTokens.response)
    public interface MyProxy extends ProxyPlace<ResponsePresenter> {
    }

    
    public interface MyView extends View, HasUiHandlers<ResponseUiHandlers> {
	void setName(String name);
    }

    private final PlaceManager placeManager;

    private String name;

    @Inject
    ResponsePresenter(EventBus eventBus, MyView view, MyProxy proxy, PlaceManager placeManager) {
	super(eventBus, view, proxy, RevealType.Root);

	this.placeManager = placeManager;

	getView().setUiHandlers(this);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
	super.prepareFromRequest(request);
	name = request.getParameter(TokenParameters.NAME, null);
	getView().setName(name);
    }

    @Override
    public void onClose() {
	PlaceRequest homePlaceRequest = new PlaceRequest.Builder().nameToken(NameTokens.home).build();
	placeManager.revealPlace(homePlaceRequest);
    }

}
