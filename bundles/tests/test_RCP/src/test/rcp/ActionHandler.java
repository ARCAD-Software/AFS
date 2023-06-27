package test.rcp;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

public class ActionHandler extends AbstractHandler {

	private void info(String message) {
		MessageDialog.openInformation(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Info", message);
	}
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		// TODO Auto-generated method stub
		
		info("Generic test message");
		
		return null;
	}

}
