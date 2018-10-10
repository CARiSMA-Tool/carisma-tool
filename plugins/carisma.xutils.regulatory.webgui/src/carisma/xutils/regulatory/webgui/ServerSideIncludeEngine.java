package carisma.xutils.regulatory.webgui;
/* 
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of Jibble Web Server / WebServerLite.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: ServerSideIncludeEngine.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

 */

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Provides static methods to offer limited support for simple SSI command
 * directives.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class ServerSideIncludeEngine {

	private static RequestThread request;

	private ServerSideIncludeEngine() {
		// Prevent this class from being constructed.
	}

	// Deliver the fully processed SSI page to the client
	public static void deliverDocument(OutputStream out, ServerFile file,RequestThread inrequest)
			throws IOException, InterruptedException {
		request = inrequest;
		HashSet<ServerFile> visited = new HashSet<ServerFile>();
		parse(out, visited, file);
	}

	// Oooh scary recursion
	private static void parse(OutputStream out, HashSet<ServerFile> visited,
			ServerFile file) throws IOException, InterruptedException {

		WebServer.log().fine("Worko on:" + file.getAbsolutePath());
		if (!file.exists() || file.isDirectory()) {
			out.write(("[SSI include not found: " + file.getCanonicalPath() + "]")
					.getBytes());
			return;
		}

		if (visited.contains(file)) {
			out.write(("[SSI circular inclusion rejected: "
					+ file.getCanonicalPath() + "]").getBytes());
			return;
		}

		visited.add(file);

		if (file.isSSI()) {
			performSSIReplacements(out, visited, file);
			
		} else if (file.isScript()) {
			// process this script
			ServerSideScriptEngine.execute(out, new HashMap<String, String>(), file, file.urlpart,file.fileparams);
		} else {
			// just dish out the bytes
			BufferedInputStream reader = new BufferedInputStream(
					new FileInputStream(file.getFile()));
			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = reader.read(buffer, 0, 4096)) != -1) {
				out.write(buffer, 0, bytesRead);
			}
		}

		visited.remove(file);

	}

	static void performSSIReplacements(OutputStream out,
			HashSet<ServerFile> visited, ServerFile file)
			throws IOException, InterruptedException {
		BufferedReader reader = new BufferedReader(new FileReader(file.getFile()));
		String line = null;
		while ((line = reader.readLine()) != null) {
			int startIndex;
			int endIndex;
			while ((line.indexOf("<!--#include file=\"") >= 0)
					|| (line.indexOf("<!--#include virtual=\"") >= 0)) {
				if ((startIndex = line.indexOf("<!--#include file=\"")) >= 0) {
					if ((endIndex = line.indexOf("\" -->", startIndex)) > startIndex) {
						out.write(line.substring(0, startIndex).getBytes());
						String filename = line.substring(startIndex + 19,
								endIndex);
						parse(out, visited, new ServerFile(file.getParentFile(),
								filename, request));
						line = line.substring(endIndex + 5, line.length());
					} else {
						line = flushRest(out, line);
					}
				} else if ((startIndex = line.indexOf("<!--#include virtual=\"")) >= 0) {
					if ((endIndex = line.indexOf("\" -->", startIndex)) > startIndex) {
						out.write(line.substring(0, startIndex).getBytes());
						String filename = line.substring(startIndex + 22,
								endIndex);
						Logger.log("SSI virtual = ",filename,0);
						ServerFile includefile = filename.indexOf("/") == 0 ? new ServerFile( WebServerConfig.root,filename,request) : new ServerFile(file.getParentFile(),
								filename,request);
						Logger.log("SSI virtual include = ",includefile.getAbsolutePath(),0);
						parse(out, visited,includefile );
						line = line.substring(endIndex + 5, line.length());
					} else {
						line = flushRest(out, line);
					}
				} else {
					line = flushRest(out, line);
				}
			}
			out.write(line.getBytes());
			out.write(WebServerConfig.LINE_SEPARATOR);
		}
	}

	private static String flushRest(OutputStream out, String line)
			throws IOException {
		out.write(line.substring(0, 19).getBytes());
		line = line.substring(19, line.length());
		return line;
	}

}