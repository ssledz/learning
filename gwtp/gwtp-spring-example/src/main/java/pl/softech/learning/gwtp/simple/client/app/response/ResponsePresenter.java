package pl.softech.learning.gwtp.simple.client.app.response;

import pl.softech.learning.gwtp.simple.client.place.NameTokens;
import pl.softech.learning.gwtp.simple.client.place.TokenParameters;
import pl.softech.learning.gwtp.simple.shared.dispatch.EchoAction;
import pl.softech.learning.gwtp.simple.shared.dispatch.EchoResult;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.inject.Inject;
import com.google.web.bindery.event.shared.EventBus;
import com.gwtplatform.dispatch.shared.DispatchAsync;
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
	void setResponse(String name);
    }

    private final PlaceManager placeManager;
    private final DispatchAsync dispatcher;

    private String message;

    @Inject
    ResponsePresenter(EventBus eventBus, MyView view, MyProxy proxy, PlaceManager placeManager, DispatchAsync dispatcher) {
	super(eventBus, view, proxy, RevealType.Root);

	this.placeManager = placeManager;
	this.dispatcher = dispatcher;

	getView().setUiHandlers(this);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
	super.prepareFromRequest(request);
	message = request.getParameter(TokenParameters.MESSAGE, null);
	getView().setResponse(message);
    }

    @Override
    public void onClose() {
	PlaceRequest homePlaceRequest = new PlaceRequest.Builder().nameToken(NameTokens.home).build();
	placeManager.revealPlace(homePlaceRequest);
    }

    @Override
    protected void onReset() {
	super.onReset();

	getView().setResponse("Waiting for response...");

	dispatcher.execute(new EchoAction(message), new AsyncCallback<EchoResult>() {
	    @Override
	    public void onFailure(Throwable caught) {
		getView().setResponse("An error occured: " + caught.getMessage());
	    }

	    @Override
	    public void onSuccess(EchoResult result) {
		getView().setResponse(result.getMessage());
	    }
	});

    }

}
