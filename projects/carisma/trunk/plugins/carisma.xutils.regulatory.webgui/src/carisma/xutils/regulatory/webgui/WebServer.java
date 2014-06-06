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
$Id: WebServer.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

*/


import java.io.*;
import java.net.*;

/**
 * The central class to the Jibble Web Server.  This is instantiated
 * by the WebServerMain class and listens for connections on the
 * specified port number before starting a new RequestThread to
 * allow connections to be dealt with concurrently.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class WebServer {
    /** the ontology helper. */
    private WebOntologyHelper soh = null;
    
    public WebServer(String rootDir, int port, final String ontologyFile) throws WebServerException {
        try {
            _rootDir = new File(rootDir).getCanonicalFile();
            soh = new WebOntologyHelper(ontologyFile);
        }
        catch (IOException e) {
            throw new WebServerException("Unable to determine the canonical path of the web root directory.");
        } catch (Exception e) {
            throw new WebServerException("Unable to load all required modules of the Webserver. Cause: " + e.getLocalizedMessage());
        }
        if (!_rootDir.isDirectory()) {
            throw new WebServerException("The specified root directory does not exist or is not a directory.");
            
        }
        _port = port;
    }
    
    public void activate() throws WebServerException {
        ServerSocket serverSocket = null;
        WebServerConfig.root = _rootDir;
        try {
            serverSocket = new ServerSocket(_port);
        }
        catch (Exception e) {
            throw new WebServerException("Cannot start the web server on port " + _port + ".");
        }
        WebServer.log().info("Start of WebServer " + WebServerConfig.root.getAbsolutePath() +":"+ _port );
        // Keep all RequestThreads within their own thread group for tidyness.
        ThreadGroup threadGroup = new ThreadGroup("HTTP Request Thread Group");
        while (_active) {
            try {
                // Pass the socket to a new thread so that it can be dealt with
                // while we can go and get ready to accept another connection.
                Socket socket = serverSocket.accept();
                RequestThread requestThread = new RequestThread(socket, _rootDir, soh);
                Thread thread = new Thread(threadGroup, requestThread);
                System.out.println("Start the thread ... ");
                thread.start();
            }
            catch (Exception e) {
                throw new WebServerException("Error processing new connection: " + e);
                
            }
        }
    }
    
    private File _rootDir;
    private int _port;
    private boolean _active = true;
    private static java.util.logging.Logger log;
    
    public static java.util.logging.Logger log() {
    	if(log == null) {
    		log = java.util.logging.Logger.getAnonymousLogger();
    		//log.addHandler(new java.util.logging.ConsoleHandler());
    	}
    	return log;
    }

}