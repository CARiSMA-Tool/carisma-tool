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
$Id: WebServerException.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

*/


/**
 * A custom exception class for the web server.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
@SuppressWarnings("serial")
public class WebServerException extends Exception {
    
    public WebServerException(String e) {
        super(e);
    }
    
}