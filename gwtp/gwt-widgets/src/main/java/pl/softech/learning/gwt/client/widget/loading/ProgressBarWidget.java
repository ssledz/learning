package pl.softech.learning.gwt.client.widget.loading;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Style.Display;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

/**
 * @author sledzs
 */
public class ProgressBarWidget extends Composite {

	interface Binder extends UiBinder<Widget, ProgressBarWidget> {
	}

	/**
	 * A formatter used to format the text displayed in the progress bar widget.
	 */
	public interface TextFormatter {
		/**
		 * Generate the text to display in the ProgressBar based on the current
		 * value.
		 * 
		 * @param bar
		 *            the progress bar
		 * @param curProgress
		 *            the current progress
		 * @return the text to display in the progress bar
		 */
		String getText(ProgressBarWidget bar, double curProgress);
	}

	private static Binder uiBinder = GWT.create(Binder.class);

	/**
	 * The bar element that displays the progress.
	 */
	@UiField
	DivElement barElement;

	/**
	 * The element that displays text on the page.
	 */
	@UiField
	DivElement textElement;

	/**
	 * The current progress.
	 */
	private double curProgress;

	/**
	 * The maximum progress.
	 */
	private final double maxProgress;

	/**
	 * The minimum progress.
	 */
	private final double minProgress;

	/**
	 * A boolean that determines if the text is visible.
	 */
	private boolean textVisible = true;

	/**
	 * The current text formatter.
	 */
	private final TextFormatter formatter;

	private HandlerRegistration resizeHandler;

	public ProgressBarWidget(double minProgress, double maxProgress) {
		this(minProgress, maxProgress, new DefaultTextFormatter());
	}

	public ProgressBarWidget(double minProgress, double maxProgress, TextFormatter formatter) {
		this.minProgress = minProgress;
		this.maxProgress = maxProgress;
		this.formatter = formatter;
		initWidget(uiBinder.createAndBindUi(this));
	}

	public double getProgress() {
		return curProgress;
	}

	/**
	 * Set the current progress.
	 *
	 * @param curProgress
	 *            the current progress
	 */
	public void setProgress(double curProgress) {
		this.curProgress = Math.max(minProgress, Math.min(maxProgress, curProgress));

		// Calculate percent complete
		int percent = (int) (100 * getPercent());
		barElement.getStyle().setWidth(percent, Unit.PCT);
		textElement.setInnerHTML(formatter.getText(this, curProgress));

		// Realign the text
		redraw();
	}

	/**
	 * Get the current percent complete, relative to the minimum and maximum
	 * values. The percent will always be between 0.0 - 1.0.
	 *
	 * @return the current percent complete
	 */
	public double getPercent() {
		// If we have no range
		if (maxProgress <= minProgress) {
			return 0.0;
		}

		// Calculate the relative progress
		double percent = (curProgress - minProgress) / (maxProgress - minProgress);
		return Math.max(0.0, Math.min(1.0, percent));
	}

	/**
	 * This method is called when the dimensions of the parent element change.
	 * Subclasses should override this method as needed.
	 *
	 * Move the text to the center of the progress bar.
	 *
	 * @param width
	 *            the new client width of the element
	 * @param height
	 *            the new client height of the element
	 */
	public void onResize(int width, int height) {
		if (textVisible) {
			int textWidth = textElement.getPropertyInt("offsetWidth");
			int left = (width / 2) - (textWidth / 2);
			textElement.getStyle().setLeft(left, Unit.PX);
		}
	}

	public void setTextVisible(boolean textVisible) {
		this.textVisible = textVisible;
		if (this.textVisible) {
			textElement.getStyle().setDisplay(Display.BLOCK);
			redraw();
		} else {
			textElement.getStyle().setDisplay(Display.NONE);
		}
	}

	@Override
	protected void onLoad() {
		DOM.setStyleAttribute(getElement(), "position", "relative");
		resizeHandler = Window.addResizeHandler(new ResizeHandler() {
			@Override
			public void onResize(ResizeEvent event) {
				redraw();
			}
		});
		redraw();
	}

	/**
	 * Redraw the progress bar when something changes the layout.
	 */
	public void redraw() {
		if (isAttached()) {
			int width = getElement().getPropertyInt("clientWidth");
			int height = getElement().getPropertyInt("clientHeight");
			onResize(width, height);
		}
	}

	@Override
	protected void onUnload() {
		resizeHandler.removeHandler();
	}

	/**
	 * Default implementation shows progress as integer number with % char e.g
	 * 45%
	 */
	private static class DefaultTextFormatter implements TextFormatter {

		@Override
		public String getText(ProgressBarWidget bar, double curProgress) {
			return (int) (100 * bar.getPercent()) + "%";
		}

	}

}
