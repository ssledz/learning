package pl.softech.learning.gwtp.simple.client.app;

import com.google.common.base.Preconditions;
import com.google.gwt.event.shared.UmbrellaException;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 */
public class LogUtil {

	private static final String INDENT = "    ";
	private static final String INDENT_AT = INDENT + "at ";

	public static String stackTraceToString(Throwable throwable) {
		return stackTraceToString(throwable, "\n");
	}
	
	public static String stackTraceToString(Throwable throwable, final String lineEnding) {

		Preconditions.checkNotNull(throwable);
		Preconditions.checkNotNull(lineEnding);

		final StringBuilder output = new StringBuilder();

		while (throwable != null) {

			output.append(throwable.toString()).append(lineEnding);

			StackTraceElement[] stackTraceElements = throwable.getStackTrace();

			if (stackTraceElements != null) {

				for (StackTraceElement element : stackTraceElements) {
					output.append(INDENT_AT).append(element.toString()).append(lineEnding);
				}

			} else {

				output.append(INDENT).append("[stack unavailable]");

			}

			throwable = throwable.getCause();

			if (throwable != null) {
				output.append("Caused by: ");
			}
		}
		return output.toString();
	}

	public static Throwable unwrap(final Throwable e) {
		if (e instanceof UmbrellaException) {
			final UmbrellaException ue = (UmbrellaException) e;
			if (ue.getCauses().size() == 1) {
				return unwrap(ue.getCauses().iterator().next());
			}
		}
		return e;
	}

}
