package carisma.xutils.regulatory.webgui;

import org.semanticweb.HermiT.debugger.commands.ExitCommand;
/* 
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of Jibble Web Server / WebServerLite.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: WebServerMain.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

*/


/**
 * This class contains the main method for the Jibble Web Server.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class WebServerMain {

    public static void main(String[] args) {
        
        String rootDir = WebServerConfig.DEFAULT_ROOT_DIRECTORY;
        int port = WebServerConfig.DEFAULT_PORT;
        String ontologyFile = "";
        if (args.length > 0) {
            ontologyFile = args[0];
        } else {
            System.err.println("No path to the ontology File found!\n" +
            		"Please enter the path where the ontology exists ...");
            System.exit(1);
        }
        
        if (args.length > 1) {
            rootDir = args[1];
        }
        
        if (args.length > 2) {
            try {
                port = Integer.parseInt(args[2]);
            }
            catch (NumberFormatException e) {
                // Stick with the default value.
            }
        }
        
        try {
            WebServer server = new WebServer(rootDir, port, ontologyFile);
            server.activate();
        }
        catch (WebServerException e) {
        	WebServer.log().log(java.util.logging.Level.SEVERE, "Error in ServerMain", e);
        }
    }

}