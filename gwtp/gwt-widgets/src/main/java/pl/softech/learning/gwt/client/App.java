package pl.softech.learning.gwt.client;

import pl.softech.learning.gwt.client.widget.loading.ProgressBarWidget;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.RootPanel;

public class App implements EntryPoint {

	public void onModuleLoad() {

		final ProgressBarWidget progressBarWidget = new ProgressBarWidget(0, 100);
		progressBarWidget.setTextVisible(true);

		Timer simulationTimer = new Timer() {
			@Override
			public void run() {
				if (progressBarWidget.getPercent() >= 1.0) {
					cancel();
				} else {
					progressBarWidget.setProgress(progressBarWidget.getProgress() + 1);
				}
			}
		};

		RootPanel.get("progressBar").add(progressBarWidget);
		
		simulationTimer.scheduleRepeating(100);
	}

}
