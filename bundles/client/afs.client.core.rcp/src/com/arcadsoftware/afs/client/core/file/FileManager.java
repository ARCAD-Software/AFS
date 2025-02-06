/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.afs.client.core.file;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;

import com.arcadsoftware.afs.client.core.internal.Activator;
import com.arcadsoftware.afs.framework.ui.plugins.LogUITools;

public class FileManager {

	public static IFile createInternalFile(final IProject project, final String filename) throws CoreException {
		final IFile file = project.getFile(filename);
		if (!file.exists()) {
			final byte[] bytes = "".getBytes();
			final InputStream source = new ByteArrayInputStream(bytes);
			file.create(source, IResource.NONE, null);
			project.refreshLocal(IResource.DEPTH_INFINITE, null);
		}
		return file;
	}

	// ID de l'editeur XML :
	// org.eclipse.wst.xml.ui.internal.tabletree.XMLMultiPageEditorPart

	public static IFile createInternalFile(final String projectFolder, final String filename) throws CoreException {
		final IProject project = createInternalProject(projectFolder);
		if (project != null) {
			final IFile file = project.getFile(filename);
			if (!file.exists()) {
				final byte[] bytes = "".getBytes();
				final InputStream source = new ByteArrayInputStream(bytes);
				file.create(source, IResource.NONE, null);
				project.refreshLocal(IResource.DEPTH_INFINITE, null);
			}
			return file;
		}
		return null;
	}

	public static IProject createInternalProject(final String projectFolder) throws CoreException {
		final IProgressMonitor monitor = new NullProgressMonitor();
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = root.getProject(projectFolder);
		if (project.exists()) {
			project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		} else {
			project.create(monitor);
		}
		project.open(monitor);
		project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		return project;
	}

	public static File createTemporaryFile(final String prefix, final String suffix) {
		File f;
		try {
			f = File.createTempFile(prefix, suffix);
			f.deleteOnExit();
			return f;
		} catch (final IOException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
		}
		return null;
	}

	public static File createTemporaryFileFormStream(final InputStream stream, final String filename) {
		try {
			final File f = createTemporaryFile("AFSTMP_", filename); //$NON-NLS-1$
			if (f!=null) {
				return loadFileFromStream(stream, f.getAbsolutePath());
			}
		} catch (final Exception e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
		}
		return null;
	}

	/**
	 * Filter the incoming array of <code>IFile</code> elements by removing any that
	 * do not currently exist in the workspace.
	 *
	 * @param files The array of <code>IFile</code> elements
	 * @return The filtered array
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static IFile[] filterNonExistentFiles(final IFile[] files) {
		if (files == null) {
			return null;
		}

		final int length = files.length;
		final ArrayList existentFiles = new ArrayList(length);
		for (int i = 0; i < length; i++) {
			if (files[i].exists()) {
				existentFiles.add(files[i]);
			}
		}
		return (IFile[]) existentFiles.toArray(new IFile[existentFiles.size()]);
	}

	private static IEditorDescriptor getEditorDescriptor(final String editorID) {
		// Try file specific editor.
		final IEditorRegistry editorReg = PlatformUI.getWorkbench().getEditorRegistry();
		if (editorID != null) {
			final IEditorDescriptor desc = editorReg.findEditor(editorID);
			if (desc != null) {
				return desc;
			}
		}

		return null;
	}

	private static IEditorInput getEditorInput(final IFileStore fileStore, final String title) {
		final IFile workspaceFile = getWorkspaceFile(fileStore);
		if (workspaceFile != null) {
			return new FileEditorInput(workspaceFile);
		}
		return new TitledFileStoreEditorInput(fileStore, title);
	}

	/**
	 * Determine whether or not the <code>IFileStore</code> represents a file
	 * currently in the workspace.
	 *
	 * @param fileStore The <code>IFileStore</code> to test
	 * @return The workspace's <code>IFile</code> if it exists or <code>null</code>
	 *         if not
	 */
	private static IFile getWorkspaceFile(final IFileStore fileStore) {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IFile[] files = root.findFilesForLocationURI(fileStore.toURI());
		files = filterNonExistentFiles(files);
		if (files == null || files.length == 0) {
			return null;
		}

		// for now only return the first file
		return files[0];
	}

	public static boolean loadFileFromStream(final InputStream stream, final File file) {
		if (stream != null) {
			FileOutputStream out = null;
			try {
				out = new FileOutputStream(file);
				final byte buf[] = new byte[1024];
				int len;
				while ((len = stream.read(buf)) > 0) {
					out.write(buf, 0, len);
				}
				return true;
			} catch (final IOException e1) {
				LogUITools.logError(Activator.getDefault().getBundle(), e1);
			} finally {
				if (out != null) {
					try {
						out.close();
					} catch (final IOException e1) {
						LogUITools.logError(Activator.getDefault().getBundle(), e1);
					}
				}
				try {
					stream.close();
				} catch (final IOException e1) {
					LogUITools.logError(Activator.getDefault().getBundle(), e1);
				}
			}
		} else {
			LogUITools.logError(Activator.getDefault().getBundle(),
					new NullPointerException("The stream must not be null.")); //$NON-NLS-1$
		}
		return false;
	}

	public static File loadFileFromStream(final InputStream stream, final String filename) {
		final File tempFile = new File(filename);
		final boolean result = loadFileFromStream(stream, tempFile);
		if (result) {
			return tempFile;
		} else {
			return null;
		}
	}

	public static IEditorPart openEditorOnFileStore(final IWorkbenchPage page, final IFileStore fileStore,
			final String title) throws PartInitException {
		return openEditorOnFileStore(page, fileStore, title, null);
	}

	public static IEditorPart openEditorOnFileStore(final IWorkbenchPage page, final IFileStore fileStore,
			final String title, final String preferredEditorID) throws PartInitException {
		// sanity checks
		if (page == null) {
			throw new IllegalArgumentException();
		}
		final IEditorInput input = getEditorInput(fileStore, title);

		String editorId;
		if (preferredEditorID != null && getEditorDescriptor(preferredEditorID) != null) {
			editorId = preferredEditorID;
		} else {
			editorId = IDE.getEditorDescriptor(fileStore.getName(), true, false).getId();
		}

		// open the editor on the file
		return page.openEditor(input, editorId);
	}

	public static void openFile(final File file) {
		openFile(file, null);
	}

	public static void openFile(final File file, final FileEditorListener listener) {
		final IFileStore fileStore = EFS.getLocalFileSystem().getStore(file.toURI());
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		try {

			final IEditorPart editor = IDE.openEditorOnFileStore(page, fileStore);
			if (listener != null) {
				listener.setEditor(editor);
				page.addPartListener(listener);
				editor.addPropertyListener(listener);
			}
		} catch (final PartInitException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
		}
	}

	public static void openFile(final File file, final FileEditorListener listener, final String title) {
		openFile(file, listener, title, null);
	}

	public static void openFile(final File file, final FileEditorListener listener, final String title,
			final String preferredEditor) {
		final IFileStore fileStore = EFS.getLocalFileSystem().getStore(file.toURI());
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		try {
			final IEditorPart editor;
			if (preferredEditor != null) {
				editor = openEditorOnFileStore(page, fileStore, title, preferredEditor);
			} else {
				editor = openEditorOnFileStore(page, fileStore, title);
			}

			if (listener != null) {
				listener.setEditor(editor);
				page.addPartListener(listener);
				editor.addPropertyListener(listener);
			}
		} catch (final PartInitException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
		}
	}

	public static void openFile(final File file, final IEditorInput facade, final FileEditorListener listener) {
		final IFileStore fileStore = EFS.getLocalFileSystem().getStore(file.toURI());
		final IFile iFile = getWorkspaceFile(fileStore);
		final FileEditorInputFacade facadeInputFile = new FileEditorInputFacade(iFile, facade);
		final IEditorDescriptor descriptor = IDE.getDefaultEditor(iFile);
		final String editorId = descriptor.getId();
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		try {
			final IEditorPart editor = IDE.openEditor(page, facadeInputFile, editorId);
			if (listener != null) {
				listener.setEditor(editor);
				page.addPartListener(listener);
				editor.addPropertyListener(listener);
			}
		} catch (final PartInitException e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
		}
	}

	public static void openTemporaryFile(final File file) {
		final FileEditorListener listener = new FileEditorListener(file) {
			@Override
			public void partClosed(final IWorkbenchPart part) {
				if (part.equals(editor)) {
					boolean ok = file.delete();
					if (!ok) {
						//
					}
				}
			}
		};
		openFile(file, listener);
	}

	public static boolean saveFile(final File sourceFile) {
		final String fileName = saveFile("");
		if (fileName != null) {
			final File targetFile = new File(fileName);
			if (targetFile.mkdirs()) {
				if (targetFile.exists()) {
					if(!targetFile.delete()) {
						//Do nothing
					}
				}

				return sourceFile.renameTo(targetFile);
			}
		}

		return false;
	}

	public static String saveFile(final String filename) {
		final Map<String, String> filters = new HashMap<>();
		filters.put("All files (*.*)", "*.*");
		return saveFile(filename, filters);
	}

	public static String saveFile(final String filename, final Map<String, String> filters) {
		return saveFile(null, filename, filters);
	}
	
	public static String saveFile(final String title, final String filename, final Map<String, String> filters) {
		final FileDialog dialog = new FileDialog(Activator.getDefault().getPluginShell(), SWT.SAVE);
		final String[] filterNames = new String[filters.size()];
		final String[] filterExtensions = new String[filters.size()];
		int i = 0;
		for (final Entry<String, String> filter : filters.entrySet()) {
			filterNames[i] = filter.getKey();
			filterExtensions[i] = filter.getValue();
			i++;
		}
		dialog.setFilterNames(filterNames);
		dialog.setFilterExtensions(filterExtensions);
		dialog.setFileName(filename);
		if(title != null) {
			dialog.setText(title);
		}
		return dialog.open();
	}

	/**
	 * Open a {@link FileDialog} to select a file and the apply the {@link Function} <b>fileProcessor</b> on the selected file.
	 * This function exists on the equivalent RAP bundle - if its signature changes, it must be changed in the RAP bundle as well.
	 * 
	 * @param title
	 * @param filters
	 * @param defaultFilename
	 * @param fileProcessor
	 * @return
	 */
	public static boolean selectAndProcessFile(final String title, final Map<String, String> filters, final String defaultFilename, final Consumer<File> fileProcessor) {
		final String filePath = saveFile(title, defaultFilename, filters);
		if(filePath != null && !filePath.isEmpty()) {
			try {
				fileProcessor.accept(new File(filePath));
				return true;
			}
			catch(Exception e) {
				LogUITools.logError(Activator.getDefault().getBundle(), e);
			}
		}
		return false;
	}
	
	public static String selectDirectory() {
		final DirectoryDialog dialog = new DirectoryDialog(Activator.getDefault().getPluginShell(), SWT.OPEN);
		return dialog.open();
	}

	public static String selectFile() {
		final FileDialog dialog = new FileDialog(Activator.getDefault().getPluginShell(), SWT.OPEN);
		final String[] filterNames = new String[] { "All Files (*)" };
		final String[] filterExtensions = new String[] { "*" };
		dialog.setFilterNames(filterNames);
		dialog.setFilterExtensions(filterExtensions);
		return dialog.open();
	}

	public static boolean unzipFile(final File f) {
		final String targetFolder = f.getParentFile().getAbsolutePath();
		// If the reading of the stream is ok, browse the
		// zip file an unzip the found entries into the project folder.
		try (final ZipInputStream zis = new ZipInputStream(new FileInputStream(f))) {
			// get the zipped file list entry
			final byte[] buffer = new byte[1024];
			ZipEntry ze = zis.getNextEntry();
			while (ze != null) {
				final String fileName = ze.getName();
				final File unZippedFile = new File(targetFolder, fileName);
				if (!unZippedFile.getAbsolutePath().startsWith(targetFolder)) {
					LogUITools.logError(Activator.getDefault().getBundle(), "Invalid Archive containt: " + fileName);
					return false;
				}
				try (FileOutputStream fos = new FileOutputStream(unZippedFile)) {
					int len;
					while ((len = zis.read(buffer)) > 0) {
						fos.write(buffer, 0, len);
					}
				}
				ze = zis.getNextEntry();
			}
			zis.closeEntry();
			return true;
		} catch (final Exception e) {
			LogUITools.logError(Activator.getDefault().getBundle(), e);
			return false;
		}

	}
}
