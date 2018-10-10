package carisma.xutils.regulatory.importer.juris;

import java.io.File;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

/**
 * this class is used to locally check a html-file against a DTD
 * if other files have been add, probably the systemid has to be checked again
 * 
 * @author dbuerger
 *
 */
public class JurisEntityResolver implements EntityResolver {
	private String resourcePath = null;
	
	/**
	 * creates a new Entity Resolver Object
	 * @param resourcePath the path to the type definitions
	 */
	public JurisEntityResolver (String resourcePath) {
		super();
		this.resourcePath = resourcePath;
	}

	public InputSource resolveEntity (String publicId, String systemId)
    {
        if (systemId.endsWith(".dtd") || systemId.endsWith(".ent")) {
        	File fileToResolve = new File(systemId);
        	return new InputSource(resourcePath + File.separator + fileToResolve.getName());
        }
        return null;
    }
}

