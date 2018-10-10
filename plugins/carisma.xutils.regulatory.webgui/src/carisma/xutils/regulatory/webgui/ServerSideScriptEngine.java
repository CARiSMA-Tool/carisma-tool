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
$Id: ServerSideScriptEngine.java,v 1.4 2004/02/01 13:37:35 pjm2 Exp $

 */

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

/**
 * Provides limited support for running server side scripts. The HashMap of
 * server variables are sent to the process when it is executed. While the
 * process is outputting data to standard output, this will be issued to the
 * connecting client.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class ServerSideScriptEngine {

	// This could be a lot better. Consider server side scripting a beta feature
	// for now.

	public static void execute(OutputStream out,
			Map<String, String> serverVars, ServerFile file, String path,
			String params) throws IOException, InterruptedException {

		Process process = createProcess(serverVars, file, params);

		InputStream in = new BufferedInputStream(process.getInputStream(),
				2 * 8192);
		// Send the process output to the connecting client.
		// InputStream in = new FileInputStream(result.getAbsolutePath());
		// cgi MUSS DAS SELBER MACHEN
		byte[] mibuffer = new byte[26];
		if ((in.read(mibuffer, 0, 26)) != 26) { // kein Contenttype
			Logger.log("", "Kein Header", 1);
			return;
		}

		char[] cbuffer = new char[8192];
		java.io.InputStreamReader re = new java.io.InputStreamReader(
				new BufferedInputStream(process.getErrorStream(), 2 * 8192));

		// WebServer.log().info("Exitcode: " + process.exitValue());
		byte[] buffer = new byte[8192];
		int bytesRead1, bytesRead2;
		for (int k = 0; k < 100; k++) {
			while (in.available() > 0 || re.ready()) {
				if ((bytesRead1 = in.read(buffer, 0, 8192)) != -1) {
					out.write(buffer, 0, bytesRead1);
					System.out.println("Data: " + bytesRead1);
				}

				if ((bytesRead2 = re.read(cbuffer, 0, 8192)) != -1) {
					// message.write(cbuffer, 0, bytesRead2);

					System.out.println("Err: " + bytesRead2);
					System.out.println(new String(cbuffer));
				}
			}
			Runtime.getRuntime().gc();
		}
		in.close();
	}

	private static Process createProcess(Map<String, String> serverVars,
			ServerFile file, String params) throws IOException {
		ProcessBuilder pb;

		// Build the process
		String filename = file.getAbsolutePath();
		// File result = File.createTempFile("perlout_", ".html");
		// File err = File.createTempFile("perlout_", ".err");
		if (filename.toLowerCase().endsWith(".pl")) {
			pb = new ProcessBuilder("perl", "-I.", filename, "QUERY_STRING="
					+ params);
		} else if (filename.toLowerCase().endsWith(".php")) {
			pb = new ProcessBuilder("php", filename);
		} else {
			pb = new ProcessBuilder(filename);
		}

		// Build Enviroment
		Map<String, String> env = pb.environment();
		env.putAll(serverVars);
		env.put("DOCUMENT_ROOT", WebServerConfig.root.getCanonicalPath());
		env.put("DOCUMENT_URI", file.getRequestURI());
		pb.directory(file.getParentFile());
		Process process = pb.start();
		return process;
	}
}