package pl.softech.learning.gwtp.tab.client.app.ui.tabs;

import com.google.gwt.resources.client.CssResource;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.Tab;
import com.gwtplatform.mvp.client.TabData;

/**
 * This is a basic implementation of a {@link Tab} that must be subclassed. Subclasses can style the tab in any way they
 * want. A {@link BaseTab} is added to a {@link BaseTabPanel}.
 */
public abstract class BaseTab extends Composite implements Tab {
    
    protected interface Style extends CssResource {
        String active();

        String inactive();
    }

    @UiField
    Hyperlink hyperlink;
    @UiField
    Style style;

    private final float priority;

    public BaseTab(TabData tabData) {
        super();

        this.priority = tabData.getPriority();
    }

    @Override
    public void activate() {
        removeStyleName(style.inactive());
        addStyleName(style.active());
    }

    @Override
    public Widget asWidget() {
        return this;
    }

    @Override
    public void deactivate() {
        removeStyleName(style.active());
        addStyleName(style.inactive());
    }

    @Override
    public float getPriority() {
        return priority;
    }

    @Override
    public String getText() {
        return hyperlink.getText();
    }

    @Override
    public void setTargetHistoryToken(String historyToken) {
        hyperlink.setTargetHistoryToken(historyToken);
    }

    @Override
    public void setText(String text) {
        hyperlink.setText(text);
    }

    /**
     * Checks whether or not the current user has the right to access this tab. By default, all tabs can be accessed.
     *
     * @return {@code true} if the user can access this tab, {@code false} otherwise.
     */
    public boolean canUserAccess() {
        return true;
    }
}
