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
$Id: RequestThread.java,v 1.2 2004/02/01 13:37:35 pjm2 Exp $

 */

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.OntologyHelperException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

/**
 * A thread which deals with an individual request to the web server. This is
 * passed a socket from the WebServer when a connection is accepted.
 * 
 * @author Copyright Paul Mutton, http://www.jibble.org/
 */
public class RequestThread implements Runnable {

	    /** the request. */
	private String mainRequest;
	    /** the ontology helper. */
	private WebOntologyHelper soh = null;
	    /** to switch between the declarations of the fields. */
	private enum CreateRuleElement { storeRuleElement, className, elName, elType, epos, spos }
	    /** to switch between the different types of individuals.*/ 
	private enum GetTheIndividuals { BSIMeasure, BSIThread, BSIElement, Law, Paragraph, Section, MARiskComment, MARiskBinding, }
	    /** to switch between the information on a deletion request. */
	private enum DeleteRuleElement { element, name, spos, epos }
	    /** to handle the different requests */
	private enum HandleTheGeneralRequest { storeSituation, storeRuleElement, storeOntology, delete, Constraint, relatedRuleElements,
		References, Situation, RuleElements, ConstraintSpecification, RelatedRuleElements, checkRuleElements} 
	    
	
	
	/**
	 * initializes a new thread for each connection. 
	 * @param socket the socket
	 * @param rootDir the rootDir
	 */
	public RequestThread(final Socket socket, final File rootDir, WebOntologyHelper soh) {
		gsocket = socket;
		grootDir = rootDir;
//		lock = new ReentrantLock(true);
		this.soh = soh;
	}

	// handles a connection from a client.
	/** (non-Javadoc).
	 * @see java.lang.Runnable#run()
	 */
	public final void run() {
		String ip = "unknown";
		
		String request = "unknown";
		BufferedInputStream reader = null;
		String[] changes = null;
		try {
			ip = gsocket.getInetAddress().getHostAddress();
			BufferedReader in = new BufferedReader(new InputStreamReader(
					gsocket.getInputStream()));
			OutputStream out = new HTMLOutWriter(new BufferedOutputStream(
					gsocket.getOutputStream()));

			String path = "";
			// Read the first line from the client.
			request = in.readLine();
			if (request != null) {
			    request = URLDecoder.decode(request, "UTF-8");
			}
			if (request != null
					&& request.startsWith("GET ")
					&& (request.endsWith(" HTTP/1.0") || request 
							.endsWith("HTTP/1.1"))) {
				path = request.substring(4, request.length() - 9);
				try {
				    System.out.println("Request: " + request);
					request = request.substring(request.indexOf("?") + 1, request.length() - 9);
		            mainRequest = request;
					changes = request.split("&");
					if (request.contains("=")) {
					    request = request.substring(0, request.indexOf("="));
					}
						// if there are no changes delivered, we have to do nothing
				} catch (IndexOutOfBoundsException i) {
					System.out.println("The request does not contain any variables!");
					changes = null;
				}
			} else {
				// invalid request
				Logger.log(ip, request, 405);
				gsocket.close();
				return;
			}

			// Read in and store all the headers.
			HashMap<String, String> headers = new HashMap<String, String>();
			String line = null;
			while ((line = in.readLine()) != null) {
				line = line.trim();
				if (line.equals("")) {
					break;
				}
				int colonPos = line.indexOf(":");
				if (colonPos > 0) {
					String key = line.substring(0, colonPos);
					String value = line.substring(colonPos + 1);
					headers.put(key, value.trim());
				}
			}
				// check if only part of the page was requested
			if (path.contains("?") && path.contains("WebOntology.html")) {
				handleRequestForWebOntology(path, changes, request, out);
				out.close();
				return;
			} else {
				ServerFile.setRootDir(grootDir);
				ServerFile file = new ServerFile(URLDecoder.decode(path, "UTF-8"), this);
	
				if (!file.isInRootSystem()) {
					forbidden(ip, request, out, path);
					out.close();
					return;
				}
	
				if (file.isDirectory()) {
					// Check to see if there are any index files in the directory.
					file = searchIndexFile(file);
					if (file.isDirectory()) {
						// print directory listing
						returnDirListing(ip, request, out, path, file);
						out.close();
						return;
					}
				}
	
				if (!file.exists()) {
					fileNotFound(ip, request, out, path);
					out.close();
					return;
				}
	
				if (file.isCGI()) {
					executeScrips(ip, request, out, path, headers, file);
					out.close();
					return;
				}
				reader = deliverPage(ip, request, out, file);
			}
			out.close();
		} catch (Exception e) {
			WebServer.log().severe("[" + ip + "] ERROR " + e.toString() + " " + request);
			e.printStackTrace();
			if (reader != null) {
				try {
					reader.close();
				} catch (Exception anye) {
					anye.printStackTrace();
				}
			}
		} 
		WebServer.log().fine("[" + ip + "] Request finished");
	}
	
	/**
	 * deals with the requests for the WebOntology.html
	 * @param path the path which was requested
	 * @param changes the changes which have to be stored
	 * @param out the Outputstream
	 * @throws IOException 
	 */
	private final void handleRequestForWebOntology(final String path, final String[] changes, final String request, final OutputStream out) throws IOException {
	    String[] partsOfThePage = path.substring(path.indexOf("?") + 1, path.length()).split("&");
	    String requestValue = "";
	    if (request.contains("&")) { // test whether the request contains 'delete&name'
	        requestValue = request.substring(0, request.indexOf("&"));
	    } else {
	        requestValue = request;
	    }
	    
	    if (doesEnumContainString(HandleTheGeneralRequest.class, requestValue)) {
	        handleGeneralRequest(requestValue, out, changes, partsOfThePage);
	    } else if (doesEnumContainString(GetTheIndividuals.class, requestValue)) {
	        handleLawContentRequest(mainRequest, partsOfThePage, out);	   
	    } else if (Arrays.asList(WebOntologyHelper.ruleElementClasses).contains(requestValue)){
	        loadRuleElements(requestValue, out);
	    } else {
	        throw new UnsupportedOperationException("No operation supports the request: " + mainRequest);
	    }
	    ((HTMLOutWriter) out).sendDoc("text/html", new Date(System.currentTimeMillis()).toString());
        out.flush();
	}
	
	/**
	 * checks if the given enum class contains a specific string
	 * @param enumType the class
	 * @param s the string
	 * @return true if the enum contains the string
	 */
	private <T extends Enum<T>> boolean doesEnumContainString(Class<T> enumType, String s) {
	    for (Field f : enumType.getDeclaredFields()) {
            if(f.getName().equals(s)) return true;
        }
        return false;
    }
	
	/**
	 * handles the general request for fields in the main website
	 * @param out the Outpustream
	 * @param changes the changes 
	 * @param partsOfThePage the parts 
	 * @throws IOException
	 */
	private final void handleGeneralRequest(final String request, final OutputStream out, final String[] changes, final String[] partsOfThePage) throws IOException {
	    switch (HandleTheGeneralRequest.valueOf(request)) {
            case storeSituation:
                storeSituation(mainRequest.split("\\?"), out);
                break;
            case storeRuleElement:
                if (changes.length != 6) {
                    throw new IOException("The Request was corrupt! " + request + "-" +Arrays.toString(changes));
                }
                confirmChanges(changes, out);
                break;
            case storeOntology:
                storeOntology(out);
                break;
            case Constraint:
                handleConstraint(changes, out);
                break;
            case References:
                handleReferences(out, partsOfThePage);
                break;
            case Situation:
                handleSituations(out);
                break;
            case RuleElements:
                handleRuleElements(out);
                break;
            case delete:
                deleteRuleElement(partsOfThePage, out);
                break;
            case ConstraintSpecification:
                handleConstraint(changes, out);
                break;
            case RelatedRuleElements:
            	handleRelatedRuleElements(out);
            	break;
            case checkRuleElements:
            	handleCheckOfRuleElements(out);
            	break;
            case relatedRuleElements:
            	handleRequestForRelatedRuleElements(out);
            	break;
        }  
	}
	
	
	private void handleRequestForRelatedRuleElements(OutputStream out) throws IOException {
		if (!mainRequest.contains("=")) {
			out.write("error".getBytes());
			out.flush();
		} else {
			String ruleElement = mainRequest.split("=")[1];
			out.write(soh.getRelatedRuleElements(ruleElement.split(",")).getBytes());
			out.flush();
		}
		
	}

	/**
	 * handles the check of the ruleelements.
	 * @param out
	 * @throws IOException
	 */
	private void handleCheckOfRuleElements(OutputStream out) throws IOException {
		if (!mainRequest.contains("&")) {	// here no ruleelements are present
			out.write("error".getBytes());
			out.flush();
		} else {
			String[] ruleelements = mainRequest.split("=")[1].split(",");
			List<String> checked = soh.checkSelectedRuleElements(ruleelements);
			String result = "";
			for (String s : checked) {
				result = result + "," + s;
			}
			out.write(result.getBytes());
			out.flush();
		}
		
	}

	/**
	 * handles the request for rule elements
	 * @param out the Outputstream
	 * @throws IOException
	 */
	private final void handleRuleElements(final OutputStream out) throws IOException {
	    if (!mainRequest.contains("=")) {
            out.write(soh.getAllRuleElements().getBytes());
        } else {
            String ruleName = mainRequest.substring(mainRequest.indexOf("=") + 1);
            out.write(soh.getRuleElementsForSpecificRule(ruleName).getBytes());
        }
	}
	
		
	/**
	 * handles the request for generating new ruleelement relations.
	 * @param out
	 * @throws IOException
	 */
	private final void handleRelatedRuleElements(final OutputStream out) throws IOException {
		String[] relatedRuleElements = mainRequest.substring(mainRequest.indexOf("=") + 1).split("/");
		out.write(soh.createRuleElementRelation(relatedRuleElements).getBytes());
		soh.storeOntology();
	}
	
	/**
	 * handle the request for situations 
	 * @param out the Outputstream
	 * @throws IOException
	 */
	private void handleSituations(final OutputStream out) throws IOException {
	    if (mainRequest.contains("=")) {
            String ruleName = mainRequest.substring(mainRequest.indexOf("=") + 1);
            out.write(soh.getSituations(ruleName).getBytes());
        } else {
            throw new UnsupportedOperationException("Situations allways relate to a rule! Request: " + mainRequest);
        }
	}
	
	/**
	 * handles the references request from the website
	 * @param out the Outputstream
	 * @param partsOfThePage the array
	 * @throws IOException 
	 */
	private void handleReferences(final OutputStream out, final String[] partsOfThePage) throws IOException {
	    out.write(soh.getReferences(partsOfThePage[0].substring(partsOfThePage[0].indexOf("=") + 1)).getBytes());
  	}
	/**
	 * stores the ontology
	 * @param out the outputstream
	 * @throws IOException
	 */
	private void storeOntology(final OutputStream out) throws IOException {
	    try {
            soh.storeOntology();
            out.write("succesful".getBytes());
        } catch (OntologyHelperException o) {
            out.write("error".getBytes());
            System.err.println("Failed to store the ontology! Cause: " + o.getLocalizedMessage());
        }
	}
	/**
	 * extracts the deletion details from the request and delivers them to the Web Server Ontology Helper.
	 * @param parts the parts of the request
	 * @param out the OutputStream
	 * @throws IOException 
	 */
	private void deleteRuleElement(String[] parts, OutputStream out) throws IOException {
        String element = ""; String startPos = ""; String endPos = ""; String value = "";
        String response = "";
        try {
            for ( int i = 0; i < parts.length; i++) {
                String req = "";
                if (!parts[i].equals("delete")) {
                    req = parts[i].substring(0, parts[i].indexOf("="));
                    switch (DeleteRuleElement.valueOf(req)) {
                    case element:
                        element = parts[i].substring(parts[i].indexOf("=") + 1);
                        break;
                    case name:
                        value = parts[i].substring(parts[i].indexOf("=") + 1);
                        break;
                    case spos:
                        startPos = parts[i].substring(parts[i].indexOf("=") + 1);
                        break;
                    case epos:
                        endPos = parts[i].substring(parts[i].indexOf("=") + 1);
                        break;
                    }
                }
            }
            if (!element.equals("") && !value.equals("") && !startPos.equals("") && !endPos.equals("")) {
                response = soh.deleteElement(element, value, startPos, endPos);
                out.write(response.getBytes());
            } else {
                out.write("error".getBytes());
            }
        } catch (IllegalArgumentException il) { 
            System.err.println("There was no matching enum constant found ...");
            il.printStackTrace();
        } catch (IndexOutOfBoundsException in) { 
            System.err.println("The request was corrupt ...");
            in.printStackTrace();
        }
	}

	/**
	 * the sub elements of the ruleelement classes will be loaded
	 * @param request the request
	 * @param out the OutputStream
	 * @throws IOException 
	 */
	private void loadRuleElements(final String request, final OutputStream out) throws IOException {
         out.write(soh.getSubElementsOfRe(request, "").getBytes());    	
	}
	
	/**
	 * stores the new situation
	 * @param parts the members of the situation
	 * @param out
	 * @throws IOException 
	 */
	private void storeSituation(final String[] parts, final OutputStream out) throws IOException {
	    out.write(soh.createSituation(parts).getBytes());
	}
	
	/**    
	 * handles the request for laws. 
	 * Therefore it has to be distinguished between the different law types.
	 * @param request
	 * @param parts
	 * @param out
	 * @return
	 * @throws IOException
	 */
	private void handleLawContentRequest(final String request, final String[] parts, final OutputStream out) throws IOException {
	    Set<OWLNamedIndividual> individuals = null;
	    String reqElem = "";
	    try {
	        reqElem = request.substring(0, request.indexOf("="));
	    } catch (IndexOutOfBoundsException i) {
	        reqElem = request;
	    }
	    String ruleName = "";
	    try {
    	    switch (GetTheIndividuals.valueOf(reqElem)) {
            case MARiskBinding:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKBINDING);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT).getBytes());
                }
                break;
            case MARiskComment:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT).getBytes());
                }
                break;
            case Law:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW); 
                } else {
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_RULE_TITLE).getBytes());
                }
                break;
            case Section:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_SECTION);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_SECTION_CONTENT).getBytes());
                }
                break;
            case BSIElement:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIELEMENT);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT).getBytes());
                }
                break;
            case BSIMeasure:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIMEASURE);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT).getBytes());
                }
                break;
            case BSIThread:
                if (!request.contains("=")) {
                    individuals = soh.getIndividuals(RegulatoryOntologyHelper.CLS_BSITHREAT);
                } else { 
                    ruleName = request.substring(request.indexOf("=") + 1);
                    out.write(soh.getText(ruleName, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT).getBytes());
                }
                break; 
            
            default:
                break;
            }
    	    if (individuals != null) {
                getDeclarations(individuals, out);
            }
	    } catch (IllegalArgumentException ia) {
            System.err.println("There was no matching enum constant found...");
            ia.printStackTrace();
        }
	}
	
	/**
	 * handles the constraint requests
	 * @param parts the parts of the constraints (e.g. name and ruleelements)
	 * @param out the outputstream
	 * @throws IOException 
	 */
	private void handleConstraint(final String[] parts, final OutputStream out) throws IOException {
	    try {
	        if (parts[0].contains("ConstraintSpecification")) {
	            out.write(soh.getConstraintSpecification(parts[0].substring(parts[0].indexOf("=") + 1)).getBytes());
	        } else if (parts[0].contains("=")) {
    	        String name = mainRequest.substring(mainRequest.indexOf("=") + 1, mainRequest.indexOf("&"));
        	    out.write(soh.createConstraintClass(name, parts).getBytes());
    	    } else {
    	        out.write(soh.getConstraintClasses().getBytes());
    	    }
	    } catch (IndexOutOfBoundsException i) {
	        System.err.println("Corrupt Request submitted. " + Arrays.toString(parts));
	    }
	}
	
	/**
	 * catches all the related declarations from the given set of individuals 
	 * @param individuals the set
	 * @param out the outputstream
	 * @throws IOException 
	 */
	private void getDeclarations(Set<OWLNamedIndividual> individuals, OutputStream out) throws IOException {
	    String response = "";
        for (OWLNamedIndividual individual : individuals) {
            response = response + individual.getIRI().getFragment() + "?";
        }
        out.write(response.getBytes());
	}

	/**
	 * delivers the page.
	 * @param ip the IPAdress
	 * @param request the request
	 * @param out the Outputstream
	 * @param file the file
	 * @return a buffered Input Stream
	 * @throws IOException when an I/O error occured
	 * @throws InterruptedException when the 
	 */
	private BufferedInputStream deliverPage(final String ip, final String request,
			final OutputStream out, final ServerFile file) throws IOException, InterruptedException {
		this.mainRequest = file.urlpart;
		String extension = file.getExtension();
		BufferedInputStream reader = new BufferedInputStream(
				new FileInputStream(file.getFile()));
		Logger.log(ip, request, 200);
		String contentType = (String) WebServerConfig.MIME_TYPES.get(extension);
		if (contentType == null) {
			contentType = "application/octet-stream";
		}

		if (WebServerConfig.SSI_EXTENSIONS.contains(extension)) {
			ServerSideIncludeEngine.deliverDocument(out, file, this);
			((HTMLOutWriter) out).sendDoc(contentType, new
			 Date(file.lastModified()).toString());
			 gsocket.close();

		} else {

			byte[] buffer = new byte[4096];
			int bytesRead;
			while ((bytesRead = reader.read(buffer, 0, 4096)) != -1) {
				out.write(buffer, 0, bytesRead);
			}
			((HTMLOutWriter) out).sendDoc(contentType,
					new Date(file.lastModified()).toString());
			out.flush();
			reader.close();
			gsocket.close();
		}
		return reader;
	}

	/**
	 * searches for an index file.
	 * @param infile the file
	 * @return the file
	 * @throws IOException when an I/O error occurs 
	 */
	private ServerFile searchIndexFile(final ServerFile infile) throws IOException {
		File file = infile.getFile();
		for (int i = 0; i < WebServerConfig.DEFAULT_FILES.length; i++) {
			File indexFile = new File(file, WebServerConfig.DEFAULT_FILES[i]);
			if (indexFile.exists() && !indexFile.isDirectory()) {
				file = indexFile;
				break;
			}
		}
		return new ServerFile(file, this);
	}

	/**
	 * returns the directory listing.
	 * @param ip the IPAdress
	 * @param request the request
	 * @param out the Outputstream
	 * @param path the path
	 * @param file the file
	 * @return the new path
	 * @throws IOException when an I/O error occurs
	 */
	private String returnDirListing(final String ip, final String request,
			final OutputStream out, final String path, final ServerFile file) throws IOException {
	    String pfad = "";
		Logger.log(ip, request, 200);
		if (!path.endsWith("/")) {
			pfad = path + "/";
		}
		File[] files = file.listFiles();
		out.write(("HTTP/1.0 200 OK\r\n"
				+ "Content-Type: text/html\r\n"
				+ "Expires: Thu, 01 Dec 1994 16:00:00 GMT\r\n"
				+ "\r\n"
				+ "<h1>Directory Listing</h1>"
				+ "<h3>"
				+ pfad
				+ "</h3>"
				+ "<table border=\"0\" cellspacing=\"8\">"
				+ "<tr><td><b>Filename</b><br></td><td align=\"right\"><b>Size</b></td><td><b>Last Modified</b></td></tr>" 
				+ "<tr><td><b><a href=\"../\">../</b><br></td><td></td><td></td></tr>").getBytes());
		for (int i = 0; i < files.length; i++) {
			File ifile = files[i];
			if (file.isDirectory()) {
				out.write(("<tr><td><b><a href=\"" + pfad + ifile.getName()
						+ "/\">" + ifile.getName() + "/</a></b></td><td></td><td></td></tr>")
						.getBytes());
			} else {
				out.write(("<tr><td><a href=\"" + pfad + ifile.getName()
						+ "\">" + ifile.getName()
						+ "</a></td><td align=\"right\">" + file.length()
						+ "</td><td>"
						+ new Date(file.lastModified()).toString() + "</td></tr>")
						.getBytes());
			}
		}
		out.write(("</table><hr>" + "<i>" + WebServerConfig.VERSION + "</i>")
				.getBytes());
		out.flush();
		gsocket.close();
		return pfad;
	}

	/**
	 * declines the connection.
	 * @param ip the IPAdress
	 * @param request the request
	 * @param out the Outputstream
	 * @param path the path
	 * @throws IOException when an I/O error occurs
	 */
	private void forbidden(final String ip, final String request, final OutputStream out,
			final String path) throws IOException {
		// Uh-oh, it looks like some lamer is trying to take a peek
		// outside of our web root directory.
		Logger.log(ip, request, 403);
		((HTMLOutWriter) out).send403("<h1>403 Forbidden</h1><code>" + path + "</code><p><hr>");
		return;
	}

	/**
	 * to execute a script.
	 * @param ip the IPAdress
	 * @param request the request
	 * @param out the Outputstream
	 * @param path the path
	 * @param headers the headers
	 * @param file the file
	 * @throws IOException when an I/O error occurs
	 */
	private void executeScrips(final String ip, final String request, final OutputStream out,
			final String path, final HashMap<String, String> headers, final ServerFile file)
			throws IOException {
		try {
			ServerSideScriptEngine.execute(out, headers, file, path, "");
			((HTMLOutWriter) out).sendDoc("", new
					 Date(file.lastModified()).toString());
			Logger.log(ip, path, 200);
		} catch (Throwable t) {
			// Internal server error!
			Logger.log(ip, request, 500);
			out.write(("Content-Type: text/html\r\n\r\n"
					+ "<h1>Internal Server Error</h1><code>"
					+ path
					+ "</code><hr>Your script produced the following error: -<p><pre>"
					+ t.toString() + "</pre><hr><i>" + WebServerConfig.VERSION + "</i>")
					.getBytes());
			t.printStackTrace();
		} finally {
			out.flush();
			gsocket.close();
		}

	}

	/**
	 * if the file was not found.
	 * @param ip the IPAdress
	 * @param request the request
	 * @param out the Ouputstream
	 * @param path the path
	 * @throws IOException when an I/O error occurs
	 */
	private void fileNotFound(final String ip, final String request, final OutputStream out,
			final String path) throws IOException {
		// The file was not found.
		Logger.log(ip, request, 404);
		((HTMLOutWriter) out).send404("<h1>404 File Not Found</h1><code>" + path + "</code><p><hr>");
		return;
	}

	/**
	 * the socket.
	 */
	private Socket gsocket;
	/**
	 * the root directory.
	 */
	private File grootDir;
	/**
	 * returns the request.
	 * @return the request
	 */
	public final String getRequestURI() {
		return mainRequest;
	}

	/**
	 * confirms the changes.
	 * @param changes the array full of changes
	 * @throws IOException 
	 */
	private void confirmChanges(final String[] changes, final OutputStream out) throws IOException { 
		String type = ""; String[] arr = new String[6];
		try {
    		for (int i = 0; i < changes.length; i++) {
    			type = changes[i].substring(0, changes[i].indexOf("="));
    			switch (CreateRuleElement.valueOf(type)) {
    			case className:
    				arr[1] = changes[i].substring(changes[i].indexOf("=") + 1);
    				break;
    			case storeRuleElement:
    				arr[0] = changes[i].substring(changes[i].indexOf("=") + 1);
    				break;
    			case elName:
    				arr[2] = changes[i].substring(changes[i].indexOf("=") + 1);
    				break;
    			case elType:
    				arr[3] = changes[i].substring(changes[i].indexOf("=") + 1);
    				break;
    			case spos:
    			    arr[4] = changes[i].substring(changes[i].indexOf("=") + 1);
    			    break;
    			case epos:
    			    arr[5] = changes[i].substring(changes[i].indexOf("=") + 1);
    			default:
    				break;
    			}
    		}
    		OWLNamedIndividual individual = soh.getIndividualByName(arr[0]);
            soh.createAppropriateElement(individual, arr[1], arr[2], arr[3], arr[4], arr[5]).toString();
            ((HTMLOutWriter) out).write("succesful".getBytes());
            out.flush();
            System.out.println("succes send!");
		} catch (IndexOutOfBoundsException i) {
		    System.err.println("There was no matching enum found! " + i.getLocalizedMessage());
		    ((HTMLOutWriter) out).write("error".getBytes());
            out.flush();
		}		
	}
}