package carisma.xutils.regulatory.webgui;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;


public class HTMLOutWriter extends OutputStream {
	private File tempFile = File.createTempFile("SimpleServer", ".html");
	private FileOutputStream temp = new FileOutputStream(tempFile);
	private OutputStream nestedWriter;
	private int firstfree = 0;

 //new Date(file.lastModified()).toString();
	
	public HTMLOutWriter(OutputStream writer) throws IOException {
		 super();
		 nestedWriter = writer;
			WebServer.log().finer("Write to Temp: file:/" + tempFile.getAbsolutePath());
	}

	@Override
	public void close() throws IOException {
		nestedWriter.close();
		tempFile.delete();
	}

	@Override
	public void flush() throws IOException {
		nestedWriter.flush();
	}

	public void sendDoc( String contentType, String lastModified ) throws IOException {
		 
		  nestedWriter.write(("HTTP/1.0 200 OK\r\n" + 
		           "Date: " + new Date().toString() + "\r\n" +
		           "Server: JibbleWebServer/1.0\r\n" +
		           "Content-Type: " + contentType + "\r\n" +
		           "Expires: Thu, 01 Dec 1994 16:00:00 GMT\r\n" +
		           "Content-Length: " + firstfree + "\r\n" +
		           "Last-modified: " + lastModified + "\r\n" +
		           "\r\n").getBytes());
		  deliver();
		  this.flush();
	}
	
	public void send404(String info) throws IOException {
		nestedWriter.write(("HTTP/1.0 404 Not Found\r\n"
				+ "Content-Type: text/html\r\n"
				+ "Expires: Thu, 01 Dec 1994 16:00:00 GMT\r\n" + "\r\n"
				+ info
				+ "<i>" + WebServerConfig.VERSION + "</i>").getBytes());
		nestedWriter.flush();
		nestedWriter.close();
	}

	public void send403(String info) throws IOException {
		nestedWriter.write(("HTTP/1.0 403 Forbidden\r\n"
				+ "Content-Type: text/html\r\n"
				+ "Expires: Thu, 01 Dec 1994 16:00:00 GMT\r\n" + "\r\n"
				+ info
				+ "<i>" + WebServerConfig.VERSION + "</i>").getBytes());
		nestedWriter.flush();
		nestedWriter.close();
	}
	
	private void deliver() throws IOException {
		temp.flush();
		temp.close();
		WebServer.log().finer("Read Temp: " + tempFile.getAbsolutePath());
		BufferedInputStream reader = new BufferedInputStream(
				new FileInputStream(tempFile));
		byte[] buffer = new byte[4096];
		int bytesRead;
		while ((bytesRead = reader.read(buffer, 0, 4096)) != -1) {
			nestedWriter.write(buffer, 0, bytesRead);
		}
		reader.close();
	}
	

	@Override
	public void write(int b) throws IOException {
		firstfree++;
		temp.write(b);
	}

}
