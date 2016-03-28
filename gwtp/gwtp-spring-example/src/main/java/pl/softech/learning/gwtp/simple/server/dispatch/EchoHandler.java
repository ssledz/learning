package pl.softech.learning.gwtp.simple.server.dispatch;

import org.springframework.beans.factory.annotation.Autowired;

import pl.softech.learning.gwtp.simple.server.domain.EchoService;
import pl.softech.learning.gwtp.simple.shared.dispatch.EchoAction;
import pl.softech.learning.gwtp.simple.shared.dispatch.EchoResult;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class EchoHandler extends AbstractActionHandler<EchoAction, EchoResult> {

    @Autowired
    private EchoService echoService;

    public EchoHandler() {
	super(EchoAction.class);
    }

    @Override
    public EchoResult execute(EchoAction action, ExecutionContext ctx) throws ActionException {
	return new EchoResult(echoService.echo(action.getMessage()));
    }

    @Override
    public void undo(EchoAction action, EchoResult res, ExecutionContext ctx) throws ActionException {
    }

}
