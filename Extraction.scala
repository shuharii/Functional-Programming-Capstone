package observatory

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val temps = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8")
      .getLines()
      .map(_.split(","))
      .map(col => (col(0)+","+col(1), new LocalDate(year+"-"+col(2)+"-"+col(3)), new Temperature(col(4).toDouble)))

    val stations = Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8")
      .getLines()
      .map(_.split(","))
      .map(col => (col(0)+","+col(1), Location(col(2).toDouble,col(3).toDouble)))

    temps.map{
      x => {
        val a = stations.filter(y => y._1 == x._1)
        (x._2, a.next()._2, x._3)
      }
    }
  }.toIterable

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.map(x => (x._2,x._3))
      .groupBy(_._1)
      .map{
        x => (x._1, x._2.map(_._2).sum)
      }
  }
}
