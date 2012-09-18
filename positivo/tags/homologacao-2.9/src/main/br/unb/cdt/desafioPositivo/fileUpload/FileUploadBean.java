package br.unb.cdt.desafioPositivo.fileUpload;

/**
 * 
 */

import java.util.ArrayList;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;
import org.richfaces.event.UploadEvent;
import org.richfaces.model.UploadItem;

/**
 * Um java bean que pode ser reusado para fazer o upload  
 * de arquivos.
 * 
 * http://livedemo.exadel.com/richfaces-demo/richfaces/fileUpload.jsf
 */
@Name("fileUploadBean")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class FileUploadBean {

	private ArrayList<File> files = new ArrayList<File>();
	private boolean autoUpload = true;
	private boolean useFlash = false;
	private int uploads = 1;

	
	public int getSize() {
		if (getFiles().size() > 0) {
			return getFiles().size();
		} else {
			return 0;
		}
	}
	
	public void listener(UploadEvent event) throws Exception {
		UploadItem item = event.getUploadItem();
		File file = new File();
		file.setLength(item.getFileSize());
		file.setName(item.getFileName());
		file.setData(item.getData());
		files.add(file);
		uploads--;
	}

	public String clearUploadData() {
		files = new ArrayList<File>();		
		uploads = 5;
		return null;
	}

	public long getTimeStamp() {
		return System.currentTimeMillis();
	}

	public ArrayList<File> getFiles() {
		return files;
	}
	
	public boolean isAutoUpload() {
		return autoUpload;
	}

	public void setAutoUpload(boolean autoUpload) {
		this.autoUpload = autoUpload;
	}

	public boolean isUseFlash() {
		return useFlash;
	}

	public void setUseFlash(boolean useFlash) {
		this.useFlash = useFlash;
	}

	public int getUploads() {
		return uploads;
	}

	public void setUploads(int uploads) {
		this.uploads = uploads;
	}
}