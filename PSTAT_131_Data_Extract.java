import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class PSTAT_131_Data_Extract {

	public static void main(String[] args) throws IOException {
		int WSCount = 1, TMCount = 1;
		File whoscored = new File(
				"C:\\Users\\Roy\\Desktop\\UCSB\\Year 4\\Quarter 1\\PSTAT 131\\Final Project\\whoscored");
		FileWriter WSWriter = new FileWriter(
				"C:\\Users\\Roy\\Desktop\\UCSB\\Year 4\\Quarter 1\\PSTAT 131\\Final Project\\whoscored.csv");
		WSWriter.write("id,squad,player,rating\r\n");
		for (File WSFile : whoscored.listFiles()) {
			BufferedReader WSReader = new BufferedReader(new FileReader(WSFile));
			String WSData = new String(), WSLine = WSReader.readLine();
			while (WSLine != null) {
				WSData += WSLine;
				WSLine = WSReader.readLine();
			}
			Elements playerNames = Jsoup.parse(WSData).getElementById("statistics-table-summary")
					.getElementsByClass("iconize iconize-icon-left");
			Elements playerRatings = Jsoup.parse(WSData).getElementById("statistics-table-summary")
					.getElementsByClass("rating   sorted  ");
			for (int i = 0; i < playerNames.size(); i++) {
				WSWriter.write(WSCount + "," + WSFile.getName().substring(0, WSFile.getName().length() - 4) + ","
						+ playerNames.get(i).text() + "," + playerRatings.get(i).text() + "\r\n");
				WSCount++;
			}
			WSReader.close();
		}
		WSWriter.close();
		File transfermarkt = new File(
				"C:\\Users\\Roy\\Desktop\\UCSB\\Year 4\\Quarter 1\\PSTAT 131\\Final Project\\transfermarkt");
		FileWriter TMWriter = new FileWriter(
				"C:\\Users\\Roy\\Desktop\\UCSB\\Year 4\\Quarter 1\\PSTAT 131\\Final Project\\transfermarkt.csv");
		TMWriter.write("id,squad,player,position\r\n");
		for (File TMFile : transfermarkt.listFiles()) {
			BufferedReader TMReader = new BufferedReader(new FileReader(TMFile));
			String TMData = new String(), TMLine = TMReader.readLine();
			while (TMLine != null) {
				TMData += TMLine;
				TMLine = TMReader.readLine();
			}
			for (Element i : Jsoup.parse(TMData).getElementsByClass("inline-table")) {
				Elements player = i.select("td");
				TMWriter.write(TMCount + "," + TMFile.getName().substring(0, TMFile.getName().length() - 4) + ","
						+ player.get(1).getElementsByClass("di nowrap").get(0).text() + "," + player.get(2).text()
						+ "\r\n");
				TMCount++;
			}
			TMReader.close();
		}
		TMWriter.close();
	}
}
