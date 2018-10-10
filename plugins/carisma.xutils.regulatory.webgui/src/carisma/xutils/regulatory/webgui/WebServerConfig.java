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
$Id: WebServerConfig.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

*/


import java.io.File;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Provides configuration to the web server. This leads to a standalone
 * jar file which requires no external configuration, but perhaps it
 * may be nice one day to allow these settings to be specified
 * externally so that a bit of flexibility may be given to the user
 * (this would also reduce the class size a bit :)
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class WebServerConfig {
    
    private WebServerConfig() {
        // Prevent the default constructor from being called.
    }
    
    public static final String VERSION = "<a href=\"http://www.jibble.org\">Jibble Web Server 1.0</a> - An extremely small Java web server";
    
    public static final String DEFAULT_ROOT_DIRECTORY = ".";
    public static final int DEFAULT_PORT = 80;
    
    public static final String[] DEFAULT_FILES = new String[] {"index.html", "index.htm", "index.shtml","index_de.shtml","index.shtml","index_en.shtml", "index.shtm", "index.stm", "index.sht"};
    
    public static final byte[] LINE_SEPARATOR = "\r\n".getBytes();
    
    public static final HashSet<String> SSI_EXTENSIONS = new HashSet<String>();
    public static final HashSet<String> SCRIPT_EXTENSIONS = new HashSet<String>();
    public static final HashMap<String,String> MIME_TYPES = new HashMap<String,String>();
    public static File root = null;
    
    static {
        
        // Set up the SSI filename extensions.
        SSI_EXTENSIONS.add(".shtml");
        SSI_EXTENSIONS.add(".shtm");
        SSI_EXTENSIONS.add(".stm");
        SSI_EXTENSIONS.add(".sht");
        SSI_EXTENSIONS.add(".inc");

        // Set up the SCRIPT filename extensions.
        SCRIPT_EXTENSIONS.add(".pl");
        SCRIPT_EXTENSIONS.add(".cgi");

        // Set up the filename extension to mime type associations.
        
        String ps = "application/postscript";
        MIME_TYPES.put(".ai", ps);
        MIME_TYPES.put(".ps", ps);
        MIME_TYPES.put(".eps", ps);
        
        String rtf = "application/rtf";
        MIME_TYPES.put(".rtf", rtf);
        
        String au = "audio/basic";
        MIME_TYPES.put(".au", au);
        MIME_TYPES.put(".snd", au);
        
        String exe = "application/octet-stream";
        MIME_TYPES.put(".bin", exe);
        MIME_TYPES.put(".dms", exe);
        MIME_TYPES.put(".lha", exe);
        MIME_TYPES.put(".lzh", exe);
        MIME_TYPES.put(".exe", exe);
        MIME_TYPES.put(".class", exe);
        
        String doc = "application/msword";
        MIME_TYPES.put(".doc", doc);
        
        String pdf = "application/pdf";
        MIME_TYPES.put(".pdf", pdf);
        
        String ppt = "application/powerpoint";
        MIME_TYPES.put(".ppt", ppt);
        
        String smi = "application/smil";
        MIME_TYPES.put(".smi", smi);
        MIME_TYPES.put(".smil", smi);
        MIME_TYPES.put(".sml", smi);
        
        String js = "application/x-javascript";
        MIME_TYPES.put(".js", js);
        
        String zip = "application/zip";
        MIME_TYPES.put(".zip", zip);
        
        String midi = "audio/midi";
        MIME_TYPES.put(".midi", midi);
        MIME_TYPES.put(".kar", midi);
        
        String mp3 = "audio/mpeg";
        MIME_TYPES.put(".mpga", mp3);
        MIME_TYPES.put(".mp2", mp3);
        MIME_TYPES.put(".mp3", mp3);
        
        String wav = "audio/x-wav";
        MIME_TYPES.put(".wav", wav);
        
        String gif = "image/gif";
        MIME_TYPES.put(".gif", gif);
        
        String ief = "image/ief";
        MIME_TYPES.put(".ief", ief);
        
        String jpeg = "image/jpeg";
        MIME_TYPES.put(".jpeg", jpeg);
        MIME_TYPES.put(".jpg", jpeg);
        MIME_TYPES.put(".jpe", jpeg);
        
        String png = "image/png";
        MIME_TYPES.put(".png", png);
        
        String tiff = "image/tiff";
        MIME_TYPES.put(".tiff", tiff);
        MIME_TYPES.put(".tif", tiff);
        
        String vrml = "model/vrml";
        MIME_TYPES.put(".wrl", vrml);
        MIME_TYPES.put(".vrml", vrml);
        
        String css = "text/css";
        MIME_TYPES.put(".css", css);
        
        String html = "text/html";
        MIME_TYPES.put(".html", html);
        MIME_TYPES.put(".htm", html);
        MIME_TYPES.put(".shtml", html);
        MIME_TYPES.put(".shtm", html);
        MIME_TYPES.put(".stm", html);
        MIME_TYPES.put(".sht", html);
        
        String txt = "text/plain";
        MIME_TYPES.put(".txt", txt);
        MIME_TYPES.put(".inf", txt);
        MIME_TYPES.put(".nfo", txt);
        
        String xml = "text/xml";
        MIME_TYPES.put(".xml", xml);
        MIME_TYPES.put(".dtd", xml);
        
        String mpeg = "video/mpeg";
        MIME_TYPES.put(".mpeg", mpeg);
        MIME_TYPES.put(".mpg", mpeg);
        MIME_TYPES.put(".mpe", mpeg);
        
        String avi = "video/x-msvideo";
        MIME_TYPES.put(".avi", avi);
        
    }
    
}