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
$Id: Logger.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

*/


/**
 * A logging class which prefixes messages to the standard output with
 * human readable timestamps.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class Logger {
    
    private Logger() {
        // Prevent this class from being constructed.
    }
    
    public static void log(String ip, String request, int code) {
    	WebServer.log().fine("[" +  ip  + "] \"" + request + "\" " + code);
    }
    
    public static void logCMD(String[] cmdarray) {
           String log = "";
           	for( String x : cmdarray) {
        	   log += x + " ";
           }
           	WebServer.log().info(log);
          
    }
    
}