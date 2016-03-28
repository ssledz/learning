package pl.softech.learning.gwt.simple.client;

import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.DialogBox;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class MyDialogBox implements IsWidget {

	private final DialogBox dialogBox;
	private final Button closeButton;
	private final Label contentLabel = new Label();

	public MyDialogBox() {
		dialogBox = new DialogBox();
		closeButton = new Button("Close");
		init();
	}

	private void init() {
		closeButton.getElement().setId("closeButton");
		VerticalPanel panel = new VerticalPanel();
		panel.addStyleName("dialogVPanel");
		panel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
		panel.add(contentLabel);
		panel.add(closeButton);
		dialogBox.setWidget(panel);
	}

	public HandlerRegistration addCloseButtonClickHandler(ClickHandler handler) {
		return closeButton.addClickHandler(handler);
	}

	public void setAnimationEnabled(boolean enable) {
		dialogBox.setAnimationEnabled(enable);
	}

	public void setTitle(String text) {
		dialogBox.setText(text);
	}

	public void setText(String text) {
		contentLabel.setText(text);
	}

	@Override
	public Widget asWidget() {
		return dialogBox;
	}

	public void hide() {
		dialogBox.hide();
	}

	public void center() {
		dialogBox.center();
		closeButton.setFocus(true);
	}

}
