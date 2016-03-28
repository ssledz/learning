package pl.softech.learning.gwtp.tab.client.app.ui.tabs;

import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.TabData;

/**
 * A {@link BaseTabPanel} styled to contain {@link SimpleTab}.
 * <p/>
 * Look at {@link LinkMenu} to see how we can use this widget within a UiBinder file even though its constructor relies
 * on dependency injection.
 */
public class SimpleTabPanel extends BaseTabPanel {
    public interface Binder extends UiBinder<Widget, SimpleTabPanel> {
    }

    private final TabFactory tabFactory;

    @Inject
    SimpleTabPanel(Binder binder,
                   TabFactory tabFactory) {
        this.tabFactory = tabFactory;

        initWidget(binder.createAndBindUi(this));
    }

    @Override
    protected BaseTab createNewTab(TabData tabData) {
        return tabFactory.createSimpleTab(tabData);
    }
}
