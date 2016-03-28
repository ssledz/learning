
package pl.softech.learning.gwtp.tab.client.app.home.info;

import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewImpl;

public class InfoView extends ViewImpl implements InfoPresenter.MyView {
    interface Binder extends UiBinder<Widget, InfoView> {
    }

    @Inject
    InfoView(Binder uiBinder) {
        initWidget(uiBinder.createAndBindUi(this));
    }
}
