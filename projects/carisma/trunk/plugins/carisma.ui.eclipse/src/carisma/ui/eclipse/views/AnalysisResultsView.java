/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.ui.eclipse.views;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.AnalysisResultStatus;
import carisma.core.analysis.result.CheckResult;
import carisma.core.analysis.result.StatusType;
import carisma.ui.eclipse.CarismaGUI;


/**
 * 
 */
public class AnalysisResultsView extends ViewPart {
	
	/**
	 * The ID string.
	 */
	public static final String ID = "carisma.ui.eclipse.views.AnalysisResultsView";
	
	/**
	 * The TreeViewer.
	 */
	private TreeViewer viewer = null;
	
	//########################################################################################
	/**
	 * Constructor (empty).
	 */
	public AnalysisResultsView() {
	}

	/**
	 * @param parent Parent Composite
	 */
	@Override
	public final void createPartControl(final Composite parent) {
		viewer = new TreeViewer(parent);
		viewer.setContentProvider(new MyTreeContentProvider());
		viewer.getTree().setHeaderVisible(true);

		TreeViewerColumn pluginColumn = new TreeViewerColumn(viewer, SWT.NONE);
		pluginColumn.getColumn().setText("Analysis / Check / Messages");
		pluginColumn.getColumn().setWidth(350);
		pluginColumn.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				if (element instanceof AnalysisResult) {
					return (((AnalysisResult) element).getName());
				} else if (element instanceof CheckResult) {
					return (((CheckResult) element).getName());
				} else if (element instanceof AnalysisResultMessage) {
					return (((AnalysisResultMessage) element).getText());
				} else {
					return null;
				}
			}
			@Override
			public Image getImage(final Object element) {
				if (element instanceof AnalysisResult) {
					if (((AnalysisResult) element).getStatus().equals(AnalysisResultStatus.SUCCESS)) {
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_SUCCESSFUL_ID);
					} else if (((AnalysisResult) element).getStatus().equals(AnalysisResultStatus.RUNNING)) {
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_RUNNING_ID);
					} else {
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_ERROR_ID);
					}
				} else if (element instanceof CheckResult) {
					Image result = CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_ERROR_ID);
					if (((CheckResult) element).isSuccessful()) {
						result = CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_SUCCESSFUL_ID);
						if (((CheckResult) element).getStatus() == StatusType.WARNING) {
							result = CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_SUCCESSWARNING_ID);
						} else if (((CheckResult) element).getStatus() == StatusType.ERROR) {
							result = CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_SUCCESSERROR_ID);
						}
					} 
					return result;
				} else if (element instanceof AnalysisResultMessage) {
					switch (((AnalysisResultMessage) element).getStatus()) {
					case INFO:
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_INFO_ID);
					case WARNING:
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_WARNING_ID);
					case ERROR:
						return CarismaGUI.INSTANCE.getImageRegistry().get(CarismaGUI.IMG_ERROR_ID);
					default:
						break;
					}
				}
				return null;
			}
		});

		TreeViewerColumn modelElementColumn = new TreeViewerColumn(viewer, SWT.NONE);
		modelElementColumn.getColumn().setText("model element");
		modelElementColumn.getColumn().setWidth(100);
		modelElementColumn.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				if (element instanceof AnalysisResultMessage) {
					return ((AnalysisResultMessage) element).getModelElement();
				} else {
					return null;
				}
			}
		});

		TreeViewerColumn addInfoColumn = new TreeViewerColumn(viewer, SWT.NONE);
		addInfoColumn.getColumn().setText("add. info");
		addInfoColumn.getColumn().setWidth(100);
		addInfoColumn.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				if (element instanceof AnalysisResult) {
					return (((AnalysisResult) element).getTimestamp());
				} else if (element instanceof AnalysisResultMessage) {
					return (((AnalysisResultMessage) element).getAdditionalInformation());
				}							 
				return null;
			}
		});
		this.createActions();
		this.initContextMenu();
		update();
	}
	
	/**
	 * Initializes the ContextMenu.
	 */
	private void initContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			@Override
			public void menuAboutToShow(final IMenuManager manager) {
				Action action = new Action() {
					public void run() {
						super.run();
						//System.out.println(viewer.getSelection().toString());
						//System.out.println(((IStructuredSelection)viewer.getSelection()).getFirstElement().toString());
						AnalysisResult tmpA = null;
						CheckResult tmpP;
						AnalysisResultMessage tmpD;
						if (((IStructuredSelection) viewer.getSelection()).getFirstElement() instanceof carisma.core.analysis.result.AnalysisResult) {
							tmpA = (AnalysisResult) ((IStructuredSelection) viewer.getSelection()).getFirstElement();
						}
						if (((IStructuredSelection) viewer.getSelection()).getFirstElement() instanceof carisma.core.analysis.result.CheckResult) {
							tmpP = (CheckResult) (((IStructuredSelection) viewer.getSelection()).getFirstElement());
							tmpA = tmpP.getParent();
						}	
						if (((IStructuredSelection) viewer.getSelection()).getFirstElement() 
								instanceof carisma.core.analysis.result.AnalysisResultMessage) {
							tmpD = (AnalysisResultMessage) ((IStructuredSelection) viewer.getSelection()).getFirstElement();
							tmpP = tmpD.getParent();
							tmpA = tmpP.getParent();
						}
						CarismaGUI.INSTANCE.openReport(tmpA);
					}
				};
				action.setText("Create report for selected analysis");
				manager.add(action);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getTree());
		viewer.getTree().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
		
	}
	
	/**
	 * Insert Actions into the ActionBar.
	 */
	private void createActions() {
	    Action resetItemAction = new Action("Reset") {
			public void run() {
				CarismaGUI.INSTANCE.reset();
			};
			
		};
		resetItemAction.setText("Reset results");
		resetItemAction.setDescription("Reset results");
        ImageDescriptor deleteImage = PlatformUI.getWorkbench().
        		getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_DELETE);
        resetItemAction.setImageDescriptor(deleteImage); 
        
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        mgr.add(resetItemAction);

	}
	
	/**
	 * Calls the update() method.
	 */
	@Override
	public final void setFocus() {
		update();
	}
	
	/**
	 * Updates the viewer input.
	 */
	public final void update() {
		viewer.setInput(CarismaGUI.INSTANCE.getAnalysisResults());
	}
	
	/**
	 * Nested Class: Implements ITreeContentProvider.
	 */
	static class MyTreeContentProvider implements ITreeContentProvider {

		@Override
		public Object[] getChildren(final Object parentElement) {
			if (parentElement instanceof AnalysisResultMessage) {
				return null;
			} else if (parentElement instanceof CheckResult) {
				return (((CheckResult) parentElement).getResults().toArray());
			} else if (parentElement instanceof AnalysisResult) {
				return (((AnalysisResult) parentElement).getCheckResults()
						.toArray());
			} else {
				return null;
			}
		}

		@Override
		public Object getParent(final Object element) {
			return null;
		}

		@Override
		public boolean hasChildren(final Object element) {
			if (element instanceof AnalysisResultMessage) {
				return false;
			} else if (element instanceof CheckResult) {
				return (((CheckResult) element).getResults().size() > 0);
			} else if (element instanceof AnalysisResult) {
				return (((AnalysisResult) element).getCheckResults().size() > 0);
			} else {
				return false;
			}
		}

		@Override
		public Object[] getElements(final Object inputElement) {
			return ((List) inputElement).toArray();
		}

		@Override
		public void dispose() {
		}

		@Override
		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		}

	}

}
