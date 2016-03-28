package pl.softech.learning.gwtp.tab.client.app.home;

import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewImpl;

public class HomeTabView extends ViewImpl implements HomeTabPresenter.MyView {
    interface Binder extends UiBinder<Widget, HomeTabView> {
    }

    @UiField
    FlowPanel container;

    @Inject
    HomeTabView(Binder uiBinder) {
	initWidget(uiBinder.createAndBindUi(this));
    }

    @Override
    public void setInSlot(Object slot, IsWidget content) {
	
	if (slot == HomeTabPresenter.TYPE_SetContent) {
	    
	    container.clear();
	    container.add(content);

	} else {
	    
	    super.setInSlot(slot, content);
	    
	}
    }
}
