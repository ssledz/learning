package pl.softech.learning.gwt.simple.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;

public class App implements EntryPoint {

	public void onModuleLoad() {

		final Button clickMeButton = new Button("Click me");
		final TextBox nameField = new TextBox();
		final Label errorLabel = new Label();
		final MyDialogBox dialogBox = new MyDialogBox();
		dialogBox.setTitle("My dialog");
		dialogBox.setAnimationEnabled(true);
		dialogBox.addCloseButtonClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				dialogBox.hide();
			}
		});

		clickMeButton.addStyleName("clickMeButton");

		clickMeButton.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				dialogBox.setText("Hello " + nameField.getText());
				dialogBox.center();
			}
		});

		RootPanel.get("nameFieldContainer").add(nameField);
		RootPanel.get("clickMeButtonContainer").add(clickMeButton);
		RootPanel.get("errorLabelContainer").add(errorLabel);
	}

}
