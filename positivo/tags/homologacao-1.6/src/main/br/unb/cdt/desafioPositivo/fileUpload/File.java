package br.unb.cdt.desafioPositivo.fileUpload;
/**
 * Uma representação abstrata de arquivos, com os 
 * dados necessários para o upload de arquivos com os 
 * resulados dos concurso.
 * 
 * @author rbonifacio
 */
public class File {

	private String name;
	private String mime;
	private long length;
	private byte[] data;

	public byte[] getData() {
		return data;
	}

	public void setData(byte[] data) {
		this.data = data;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		int extDot = name.lastIndexOf('.');
		if (extDot > 0) {
			String extension = name.substring(extDot + 1);
			if ("csv".equals(extension)) {
				mime = "text/csv";
			} else {
				mime = "image/unknown";
			}
		}
	}

	public long getLength() {
		return length;
	}

	public void setLength(long length) {
		this.length = length;
	}

	public String getMime() {
		return mime;
	}
}