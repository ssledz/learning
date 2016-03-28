package pl.softech.learning.gwtp.simple.shared.dispatch;

import com.gwtplatform.dispatch.shared.UnsecuredActionImpl;

public class EchoAction extends UnsecuredActionImpl<EchoResult> {

    private String message;

    public EchoAction(String message) {
	super();
	this.message = message;
    }

    /**
     * For serialization only.
     */
    @SuppressWarnings("unused")
    private EchoAction() {
	super();
    }

    public String getMessage() {
	return message;
    }

}
