package carisma.core.analysis.result.exp;

import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;


import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import org.json.*;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;


public class dbexport {

	public static void exportXml(String json) {

		

		@SuppressWarnings("deprecation")
		HttpClient httpClient = new DefaultHttpClient();

		try {
		    HttpPost request = new HttpPost("http://212.34.151.216:8080/VppDatabaseRestApi/database/testCollection/document/521");
		    request.addHeader("Content-Type", "application/json");
		    request.addHeader("Accept","application/json");
		    
		    StringEntity params = new StringEntity(json);
		    request.setEntity(params);
		    
		    HttpResponse response = httpClient.execute(request);

		    System.out.println(response.toString());
		}catch (Exception ex) {
		    // handle exception here
		} 
			
	}

}
