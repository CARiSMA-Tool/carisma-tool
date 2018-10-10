package carisma.check.processanalysis.texttools.wortschatz;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordKind;
import carisma.processanalysis.texttools.Expander;

public class WordCache implements Expander {

	private boolean dbOpen = false;
	private Connection con = null;

	public WordCache() {
	}

	@Override
	public ArrayList<WordExtension> getWordExtensions(Word word, boolean synonymsOnly) {
		return getWordExtensions(word, synonymsOnly, false);
	}

	@Override
	public ArrayList<WordExtension> getWordExtensions(Word word, boolean synonymsOnly, boolean append) {
		// Die Rueckgabeliste initialisieren, falls nichts gefunden wird oder ein
		// interner Fehler auftritt wird eine leere Liste zurueckgeliefert
		ArrayList<WordExtension> res = new ArrayList<WordExtension>();

		// is database open?
		if (!dbOpen)
			return res;

		// Do the query
		try {
			PreparedStatement prep = con.prepareStatement("SELECT * FROM words WHERE key = '?';");
			prep.setString(1, word.getContent());
			ResultSet rs = prep.executeQuery();
			prep.close();

			while (rs.next()) {
				String syn = rs.getString("synonyme");
				res.add(new WordExtension(syn, WordKind.SYNONYME));

				// An dieser Stelle auch schauen, ob ein Wort noch nicht in der
				// DB enthalten ist. Falls noch nicht enthalten DB updaten
				if (!cacheContains(syn)) {
					uddateDB(syn);
				}
			}

			rs.close();
		} catch (SQLException e) {
			// Do nothing for now - maybe later we will log this
		} finally {
			try {
				con.close();
			} catch (SQLException e) {
				// Do nothing for now - maybe later we will log this
				// e.printStackTrace();
			}
		}

		return res;
	}

	private void uddateDB(String syn) {
		// Hier fehlt noch ein geschickter Mechanismus, um die neuen Synonyme
		// mit den anderen zu verknuepfen.

	}

	private boolean cacheContains(String syn) {
		boolean res = true;

		try {
			PreparedStatement prep = con.prepareStatement("SELECT count(key FROM words WHERE key = '?'");
			prep.setString(1, syn);
			ResultSet rs = prep.executeQuery();
			prep.close();

			if (rs.next()) {
				int count = rs.getInt(1);

				if (count > 0)
					res = true;
				else
					res = false;
			} else {
				// Es ist ein Fehler aufgetreten
				res = true;
			}
		} catch (SQLException e) {
			res = true;
		} finally {
			try {
				con.close();
			} catch (SQLException e) {
				// Do nothing for now - maybe later we will log this
				// e.printStackTrace();
			}
		}

		return res;
	}

	public boolean openDatabase(String dbName) {
		boolean res = true;

		if (!dbOpen) {
			try {
				Class.forName("org.sqlite.JDBC");
				con = DriverManager.getConnection("jdbc:sqlite:" + dbName);
				con.setAutoCommit(true);
			} catch (ClassNotFoundException e) {
				res = false;
			} catch (SQLException e) {
				res = false;
			}

			dbOpen = true;
		} else {
			res = false;
		}

		return res;
	}

	public void closeDatabase() {
		try {
			con.close();
			dbOpen = false;
		} catch (SQLException e) {
			// Do nothing for now - maybe later we will log this
		}
	}
}
