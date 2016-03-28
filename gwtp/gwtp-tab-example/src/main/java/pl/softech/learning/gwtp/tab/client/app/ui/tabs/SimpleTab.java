package pl.softech.learning.gwtp.tab.client.app.ui.tabs;

import javax.inject.Inject;

import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.assistedinject.Assisted;
import com.gwtplatform.mvp.client.TabData;

/**
 * @author sledzs
 * 
 */
public class SimpleTab extends BaseTab {

    interface Binder extends UiBinder<Widget, SimpleTab> {
    }

    @Inject
    SimpleTab(Binder uiBinder, @Assisted TabData tabData) {
	super(tabData);

	initWidget(uiBinder.createAndBindUi(this));

	setText(tabData.getLabel());
    }

}
