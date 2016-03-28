package pl.softech.learning.gwtp.tab.client.app.ui;

import pl.softech.learning.gwtp.tab.client.app.ui.tabs.SimpleTabPanel;
import pl.softech.learning.gwtp.tab.client.app.ui.tabs.TabFactory;

import com.google.gwt.inject.client.AbstractGinModule;
import com.google.gwt.inject.client.assistedinject.GinFactoryModuleBuilder;
import com.google.inject.Singleton;

/**
 * @author sledzs
 * 
 */
public class UIModule extends AbstractGinModule {
    @Override
    protected void configure() {
	bind(SimpleTabPanel.Binder.class).in(Singleton.class);
	install(new GinFactoryModuleBuilder().build(TabFactory.class));
    }
}