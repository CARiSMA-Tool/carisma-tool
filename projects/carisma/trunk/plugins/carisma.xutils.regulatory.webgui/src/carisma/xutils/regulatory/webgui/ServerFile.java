package carisma.xutils.regulatory.webgui;
import java.io.File;
import java.io.IOException;

public class ServerFile {

	/* Fields */

	String urlpart = ""; // String mit dem das Serverfile gefunden wurde
	File file = null; // Das File im FileSystem
	String fileparams = ""; // Parameter aus dem urlpart
	static File rootDir = null;
	RequestThread request = null;
	
	static void setRootDir(File inRoot) {
		if (inRoot == null)
			throw new NullPointerException("Parameter must not be NULL");
		rootDir = inRoot;
	}

	public ServerFile(String urlpart,RequestThread request) throws IOException {
		this(rootDir,urlpart,request);
	}

	ServerFile(File infile,RequestThread inrequest) throws IOException {
		file = infile.getCanonicalFile();
		request = inrequest; 
	}

	public ServerFile(File parentFile, String urlpart,RequestThread inrequest) {
		
		this.urlpart = urlpart;
		int trenner = urlpart.indexOf("?");
		if (trenner >= 0) {
			this.urlpart = urlpart.substring(0, trenner );			
			
			fileparams = urlpart.substring(trenner+1);
		};
		file = new File(parentFile, this.urlpart);
		try {
			file = file.getCanonicalFile();
		} catch (IOException e) {
			WebServer.log().log(java.util.logging.Level.WARNING, "Exception while serving File", e);
			//e.printStackTrace();
		}
		request = inrequest;
	}

//	private Map<String, String> parse(String substring) {
//		System.out.println("Substring: " + substring); //TODO: DEBUG entfernen
//		String params[] =  substring.split("&");
//		Map<String, String> paramList = new HashMap<String, String>();
//		for (int i = 0; i < params.length; i++) {
//	        System.out.println("Param: " + i + "  "+ params[i]); //TODO: DEBUG entfernen
//	        String parampair[] =  params[i].split("=");
//	        paramList.put(parampair[0], parampair[1]);
//		}
//        
//		return paramList;
//	}

	public boolean isInRootSystem() throws IOException {
		return file.toString().startsWith(rootDir.toString());
	}

	public boolean isDirectory() {
		return file.isDirectory();
	}

	public boolean exists() {
		return file.exists();
	}

	public boolean isCGI() {
		// Logger.log(ip, "Pos cgi-bin: " + file.getParent().indexOf("cgi-bin")
		// + " Parent: " + file.getParent(), 0); //Debug kann Weg
		return file.getParent().indexOf("cgi-bin") >= 0;
	}

	public boolean isSSI() {
		// Work out the filename extension. If there isn't one, we keep
		// it as the empty string ("").

		return WebServerConfig.SSI_EXTENSIONS.contains(getExtension());
	}

	public boolean isScript() {
		// Work out the filename extension. If there isn't one, we keep
		// it as the empty string ("").

		return WebServerConfig.SCRIPT_EXTENSIONS.contains(getExtension());
	}

	public File[] listFiles() {
		return file.listFiles();
	}

	public ServerFile getParentServerFile() throws IOException {
		return new ServerFile(file.getParentFile(),request);
	}

	public File getParentFile() throws IOException {
		return file.getParentFile();
	}

	public String getCanonicalPath() throws IOException {
		return file.getCanonicalPath();
	}

	public File getFile() {
		return file;
	}

	public String getAbsolutePath() {
		return file.getAbsolutePath();
	}

	public long length() {
		return file.length();
	}

	public long lastModified() {
		return file.lastModified();
	}

	// Work out the filename extension. If there isn't one, we keep
	// it as the empty string ("").
	public String getExtension() {
		String extension = "";
		String filename = file.getName();
		int dotPos = filename.lastIndexOf(".");
		if (dotPos >= 0) {
			extension = filename.substring(dotPos);
		}
		return extension.toLowerCase();
	}

	public String getRequestURI() {
		
		if(request != null) {
			return request.getRequestURI();
		} else {
		return urlpart;
		}
	}

}
