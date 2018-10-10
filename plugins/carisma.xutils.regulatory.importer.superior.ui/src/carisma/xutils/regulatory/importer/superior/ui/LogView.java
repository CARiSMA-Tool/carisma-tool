/*******************************************************************************
 * Copyright (c) 2012 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.xutils.regulatory.importer.superior.ui;

import java.util.ArrayList;

import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
//Only remove comment when the Search Box is used
//import org.eclipse.ui.forms.widgets.FormToolkit;
//import org.eclipse.swt.widgets.Label;
//import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;

import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;


/**
 * The LogView for the superior importer GUI.
 * @author jkowald
 */
public class LogView extends ViewPart {
	
	/**
	 * The ID string.
	 */
	public static final String ID = "carisma.xutils.regulatory.importer.superior.ui.LogView";
	
	/**
	 * A TableViewer to show the log entries.
	 */
	private TableViewer viewer;
	
	/**
	 * A Table object corresponding to the TableViewer. 
	 */
	private Table table;
	
	/**
	 * A set of log entries.
	 */
	private LogViewEntrySet entrySet;
	
	/**
	 * Constructor.
	 */
	public LogView() {
		super();
	}

	@Override
	public final void createPartControl(final Composite parent) {
		// Only remove comment when the Search Box is used
		//FormToolkit toolkit = new FormToolkit(parent.getDisplay());
		
		// The basic GridLayout & -Data
		GridLayout layoutParent = new GridLayout(2, false);
		layoutParent.horizontalSpacing = 5;
		layoutParent.verticalSpacing = 5;
		parent.setLayout(layoutParent);
		GridData dataParent = new GridData();
		dataParent.verticalAlignment = SWT.FILL;
		parent.setLayoutData(dataParent);
		
		// Search Label + Box
		// does not provide full functionality yet!
		/*
		Label searchLabel = new Label(parent, SWT.NONE);
	    searchLabel.setText("Search:");
		Text textSearch = toolkit.createText(parent, "");				  
		textSearch.setEditable(true);
		textSearch.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		*/
		
		// Table Viewer
		Composite child = new Composite(parent, SWT.EMBEDDED);
		viewer = new TableViewer(child, SWT.MULTI | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		GridData childData = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL);
		childData.horizontalSpan = 2;
		child.setLayoutData(childData);
		TableColumnLayout layout = new TableColumnLayout();
		child.setLayout(layout);
		
		createTableViewerColumn(layout, "Time", 15);
		createTableViewerColumn(layout, "Resource", 20);
		createTableViewerColumn(layout, "Message", 60);

		viewer.setUseHashlookup(true);
		viewer.setLabelProvider(new TableLabelProvider());
		viewer.setContentProvider(new ContentProvider());
		
		entrySet = new LogViewEntrySet(new ArrayList<LogViewEntry>(), false, viewer, parent.getDisplay());
		viewer.setInput(entrySet);
	}
	
	/**
	 * This method creates a TableViewerColumn.
	 * @param layout The column layout data
	 * @param name The column name / headline
	 * @param width The column width
	 * @return The created TableViewerColumn object
	 */
	public final TableViewerColumn createTableViewerColumn(
			final TableColumnLayout layout, 
			final String name, 
			final int width) {
		final TableViewerColumn viewerColumn = new TableViewerColumn(viewer, SWT.NONE);
		final TableColumn column = viewerColumn.getColumn();
		column.setText(name);
		column.setResizable(true);
		column.setMoveable(true);
		layout.setColumnData(column, new ColumnWeightData(width));
		return viewerColumn;
	}
	
	/**
	 * This method returns the TableViewer instance.
	 * @return The TableViewer instance
	 */
	public final TableViewer getViewer() {
		return viewer;
	}
	
	/**
	 * This method returns the LogViewEntrySet instance.
	 * @return The LogViewEntrySet instance
	 */
	public final LogViewEntrySet getLogInput() {
		return this.entrySet;
	}
	
	@Override
	public final void setFocus() {
		viewer.refresh();
		viewer.getControl().setFocus();
	}

	/**
	 * The implementation of the TableLabelProvider.
	 * @author jkowald
	 */
	private class TableLabelProvider extends LabelProvider implements ITableLabelProvider {
		@Override
		public Image getColumnImage(final Object element, final int columnIndex) {
			return null;
		}
		@Override
		public String getColumnText(final Object element, final int columnIndex) {
			LogViewEntry lve = (LogViewEntry) element;
			String result = "";
			switch(columnIndex) {
			case 0:
				result = lve.getTimestamp();
				break;
			case 1:
				result = lve.getResource();
				break;
			case 2:
				result = lve.getMessage();
				break;
			default:
				// should not reach here
				result = "";
			}
			return result;
		}
	}
	
	/**
	 * The implementation of the ContentProvider for the TableViewer.
	 * @author jkowald
	 */
	private static class ContentProvider implements IStructuredContentProvider {
		@Override
		public Object[] getElements(final Object inputElement) {
			LogViewEntrySet lves = (LogViewEntrySet) inputElement;
			return lves.getEntrySet().toArray();
		}
		@Override
		public void dispose() {
		}
		@Override
		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		}
	}
	
}
