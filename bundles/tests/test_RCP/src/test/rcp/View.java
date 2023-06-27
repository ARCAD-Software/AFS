package test.rcp;

import java.util.*;

import javax.inject.Inject;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.part.ViewPart;

public class View extends ViewPart {

	public static final String ID = "test.rcp.view";

	@Inject IWorkbench workbench;
	
	private TableViewer viewer;
	private Button button;

	private DateTime dialogDateTime;
	
	private class StringLabelProvider extends ColumnLabelProvider {
		// getText method is used from super class ColumnLabelProvider
		@Override
		public Image getImage(Object obj) {
			try {
				return workbench.getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
			} catch (NullPointerException e) {
				return null;
			}
		}
	}

	@Override
	public void createPartControl(Composite parent) {
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		viewer.getTable().setLinesVisible(true);
		TableViewerColumn column = new TableViewerColumn(viewer, SWT.NONE);
		column.setLabelProvider(new StringLabelProvider());
		viewer.getTable().getColumn(0).setWidth(200);
		viewer.setContentProvider(ArrayContentProvider.getInstance());
		// Provide the input to the ContentProvider
		viewer.setInput(createInitialDataModel());
		
		button = new Button(parent,SWT.PUSH);
		button.setText("TEST");
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				showDateChoiceDialog("test");
			}
		});

		
		
	}


	void showDateChoiceDialog(String title) {
		final Shell dialog = new Shell(Display.getCurrent(), SWT.NONE);
		dialog.setText(title);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = gridLayout.marginWidth = gridLayout.verticalSpacing = 0;
		dialog.setLayout(gridLayout);
		dialogDateTime = new DateTime(dialog, SWT.CALENDAR);
		dialogDateTime.setDate(2022, 11, 1);
		final Button ok = new Button(dialog, SWT.PUSH | SWT.CENTER);
		ok.setText("ok");
		ok.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		ok.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Calendar currentCalendar = Calendar.getInstance();
				currentCalendar.set(dialogDateTime.getYear(), dialogDateTime.getMonth(), dialogDateTime.getDay());
				//customDateTimeDate.setValue(currentCalendar.getTime());
				System.out.println("OK !");
				dialog.close();
			}
		});
		dialog.setDefaultButton(ok);
		dialog.addShellListener(new ShellListener() {
			@Override
			public void shellIconified(ShellEvent e) {}
			@Override
			public void shellDeiconified(ShellEvent e) {}
			@Override
			public void shellDeactivated(ShellEvent e) {
				dialog.close();
			}
			@Override
			public void shellClosed(ShellEvent e) {}
			@Override
			public void shellActivated(ShellEvent e) {
				dialogDateTime.setFocus();
			}
		});
		dialog.pack();
		final Point size = dialog.getSize();
		final Rectangle screen = button.getDisplay().getClientArea();
		Point pos = button.getLocation();
		pos.y += button.getSize().y;
		pos = button.getParent().toDisplay(pos);
		if ((pos.x + size.x) > screen.width) {
			pos.x = screen.width - size.x;
		}
		if ((pos.y + size.y) > screen.height) {
			pos.y -= button.getSize().y + size.y;
			if (pos.y < 0) {
				pos.y = 0;
			}
		}
		dialog.setLocation(pos);
		dialog.open();
	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}
	
	private List<String> createInitialDataModel() {
		return Arrays.asList("One", "Two", "Three");
	}
}