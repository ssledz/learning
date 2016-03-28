package pl.softech.learning.gwtp.simple.client.app;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.logging.client.HasWidgetsLogHandler;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class ApplicationView extends ViewWithUiHandlers<ApplicationUiHandlers> implements ApplicationPresenter.MyView {

	interface Binder extends UiBinder<Widget, ApplicationView> {
	}

	@UiField
	TextBox text;
	@UiField
	Button logButton;
	@UiField
	VerticalPanel logArea;

	@UiField
	ListBox loggingLevel;

	@Inject
	ApplicationView(final Binder uiBinder) {
		initWidget(uiBinder.createAndBindUi(this));

		for (Level level : Arrays.asList(Level.OFF, Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG, Level.FINE,
				Level.FINER, Level.FINEST)) {
			loggingLevel.addItem(level.getName());
		}

	}

	@Override
	public void setLoggingLevel(Level level) {

		for (int i = 0; i < loggingLevel.getItemCount(); i++) {
			if (level.getName().equals(loggingLevel.getValue(i))) {
				loggingLevel.setSelectedIndex(i);
				return;
			}
		}

	}

	@Override
	public void setLogger(final Logger logger) {
		logger.addHandler(new HasWidgetsLogHandler(logArea));
	}

	@Override
	public void resetAndFocus() {
		text.setFocus(true);
		text.selectAll();
	}

	@UiHandler("loggingLevel")
	void onLoggingLevelChanged(final ChangeEvent event) {
		getUiHandlers().setCurrentLogginLevel(Level.parse(loggingLevel.getItemText(loggingLevel.getSelectedIndex())));
	}

	@UiHandler("throwExceptionButton")
	void onThrowException(final ClickEvent event) {
		getUiHandlers().throwNPException();
	}

	@UiHandler("throwCallStackSizeExceededException")
	void onThrowCallStackSizeExceededException(final ClickEvent event) {
		getUiHandlers().throwCallStackSizeExceededException();
	}

	@UiHandler("logButton")
	void onSend(final ClickEvent event) {
		getUiHandlers().log(text.getText());
	}
}
