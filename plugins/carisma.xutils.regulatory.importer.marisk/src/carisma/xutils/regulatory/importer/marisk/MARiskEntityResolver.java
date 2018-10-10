package carisma.xutils.regulatory.importer.marisk;

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
public class MARiskEntityResolver implements EntityResolver {
	private String resourceFolderPath = null;
	
	/**
	 * creates a new Entity Resolver Object
	 * @param resourceFolderPath the path to the type definitions
	 */
	public MARiskEntityResolver (String newResourceFolderPath) {
		super();
		this.resourceFolderPath = newResourceFolderPath;
	}
	
    public InputSource resolveEntity (String publicId, String systemId)
    {
        if (systemId.endsWith(".dtd") || systemId.endsWith(".ent")) {
        	File fileToResolve = new File(systemId);
        	return new InputSource(resourceFolderPath + File.separator + fileToResolve.getName());
//        	return new InputSource(resourceFolderPath + fileToResolve.getName());
        }
        return null;
    }
}

