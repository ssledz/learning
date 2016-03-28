package pl.softech.learning.gwtp.simple.client.app.response;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;

public class ResponseView extends ViewWithUiHandlers<ResponseUiHandlers> implements ResponsePresenter.MyView {
    interface Binder extends UiBinder<Widget, ResponseView> {
    }

    @UiField
    HTML response;

    @UiField
    Button closeButton;

    @Inject
    ResponseView(Binder uiBinder) {
	initWidget(uiBinder.createAndBindUi(this));
    }

    @Override
    public void setResponse(String textToServer) {
	this.response.setHTML(textToServer);
    }

    @UiHandler("closeButton")
    void onClose(ClickEvent event) {
	getUiHandlers().onClose();
    }
}
