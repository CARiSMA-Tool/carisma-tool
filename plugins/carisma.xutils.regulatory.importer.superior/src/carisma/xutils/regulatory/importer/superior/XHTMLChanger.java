package carisma.xutils.regulatory.importer.superior;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class XHTMLChanger {
	
	public final static void main(String[] args) {
		File folder = new File("resources" + File.separator + "bsisource");
		System.out.println("File: " + folder.getAbsolutePath());
		change(folder.listFiles());
	}
	
	private static void change(final File[] files) {
		BufferedReader reader;
		BufferedWriter writer;
		for (int i = 0; i < files.length; i++) {
			System.out.println("Current: " + files[i].getName());
			if (files[i].isFile()) {
				try {
					String fileName = files[i].getAbsolutePath();
					StringBuffer buffer = new StringBuffer();
					reader = new BufferedReader(new FileReader(files[i]));
					while (reader.ready()) {
						String line = reader.readLine();
						line = changeTag(line, "img");
						line = changeTag(line, "hr");
						buffer.append(line);
					}
					if (fileName.endsWith("htm")) {
						fileName = fileName.replace("htm", "html");
						files[i].delete();
						writer = new BufferedWriter(new FileWriter(fileName));
					} else {
						writer = new BufferedWriter(new FileWriter(files[i]));
					}
					if (!buffer.toString().contains("xhtml")) {
					buffer.replace(0, buffer.indexOf("<html lang=\"de\">")
							+ "<html lang=\"de\">".length(),
							"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\" >\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"de\" lang=\"de\" >\n");
					}
					writer.write(buffer.toString());
					writer.flush();
				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IndexOutOfBoundsException e) {
					e.printStackTrace();
				}
			} else {
				change(files[i].listFiles());
			}
		}
	}

	private static String changeTag(final String line, final String tag) {
		String temp = line;
		String response = "";
		if (temp.contains("<" + tag) && !temp.contains("</" + tag + ">")) {
//			System.out.println("Input: " + temp);
			int start = temp.indexOf("<" + tag);
			String partWithoutIMG = temp.substring(start);
			int end = partWithoutIMG.indexOf(">") + 1 + start;
			String img = temp.substring(start, end);
			temp = temp.substring(0, start) + img
					+ changeTag(temp.substring(end), tag);
			response = temp.replace(img, img + " </" + tag + ">");
//			System.out.println("Output: " + response);
		} else {
			response = line;
		}
		return response;
	}
}
