package carisma.core.analysis.result.exp;

import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;

import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import java.awt.Dialog;
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
import org.eclipse.core.internal.preferences.Base64;

public class dbexport {

	public static void exportXml(String json, String id) {

		@SuppressWarnings("deprecation")
		HttpClient httpClient = new DefaultHttpClient();

		try {

			final String userCredentials = "vppapp:vppapptomcatpassword";

			String basicAuth = "Basic " + new String(new Base64().encode(userCredentials.getBytes()));
			final String baseUrl = "http://212.34.151.216:8080/VppDatabaseRestApi/database/testCollection/document/";
			// HttpPost request = new
			// HttpPost("http://212.34.151.216:8080/VppDatabaseRestApi/database/testCollection/document/777");
			String requestUrl = baseUrl + id;

			HttpPost request = new HttpPost(requestUrl);

			request.addHeader("Authorization", basicAuth);
			request.addHeader("Content-Type", "application/json");
			request.addHeader("Accept", "application/json");

			StringEntity params = new StringEntity(json);
			request.setEntity(params);

			HttpResponse response = httpClient.execute(request);
			System.out.println(response.toString());
			reaction(response, requestUrl);

			request.completed();
		} catch (Exception ex) {
			// handle exception here
		}

	}

	public static Boolean reaction(HttpResponse response, String requestUrl) {

		int status = response.getStatusLine().getStatusCode();
		System.out.println(status);
		String successful = "Entry was successful created. It can be found at: " + requestUrl;
		String warning = "There might be a problem, check whether your document was saved at: " + requestUrl;
		if (status == 201) {
			// Dialog dia = new Dialog();
			JTextArea msg = new JTextArea(successful);
			msg.setLineWrap(true);
			msg.setWrapStyleWord(true);
			JScrollPane scrollPane = new JScrollPane(msg);

			JOptionPane.showMessageDialog(null, scrollPane);
			return true;
		}

		if (status == 401) {
			JTextArea msg = new JTextArea("The user credentials are wrong.");
			msg.setLineWrap(true);
			msg.setWrapStyleWord(true);
			JScrollPane scrollPane = new JScrollPane(msg);

			JOptionPane.showMessageDialog(null, scrollPane, "Inane error", JOptionPane.ERROR_MESSAGE);
			return true;

		}

		if (status == 403) {
			JTextArea msg = new JTextArea("You don't have the permissions to submit this.");
			msg.setLineWrap(true);
			msg.setWrapStyleWord(true);
			JScrollPane scrollPane = new JScrollPane(msg);

			JOptionPane.showMessageDialog(null, scrollPane, "Inane error", JOptionPane.ERROR_MESSAGE);
			return true;

		}

		if (status == 401) {
			JTextArea msg = new JTextArea("The Collection was not found. Please inform the Administrator.");
			msg.setLineWrap(true);
			msg.setWrapStyleWord(true);
			JScrollPane scrollPane = new JScrollPane(msg);

			JOptionPane.showMessageDialog(null, scrollPane, "Inane error", JOptionPane.ERROR_MESSAGE);
			return true;

		}

		else {
			JTextArea msg = new JTextArea(warning);
			msg.setLineWrap(true);
			msg.setWrapStyleWord(true);
			JScrollPane scrollPane = new JScrollPane(msg);

			JOptionPane.showMessageDialog(null, scrollPane);
			return true;

		}
	}
}
