package pl.softech.learning.gwtp.simple.shared.dispatch;

import com.gwtplatform.dispatch.shared.Result;

public class EchoResult implements Result {

    private static final long serialVersionUID = 1L;

    private String message;

    /**
     * For serialization only.
     */
    @SuppressWarnings("unused")
    private EchoResult() {
    }

    public EchoResult(String message) {
	super();
	this.message = message;
    }

    public String getMessage() {
	return message;
    }

}
