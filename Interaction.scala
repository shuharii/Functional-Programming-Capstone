package observatory

import com.sksamuel.scrimage.{Image, Pixel, Position}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lon = tile.x/math.pow(2,tile.zoom) * 360 - 180
    val lat = math.atan(math.sinh(math.Pi - tile.y/math.pow(2,tile.zoom) * 2 * math.Pi))
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val tiles = (0 until 256 * 256).map(x => Tile(90-(x/360) + tile.x * 256, (x%360)-180 + tile.y * 256 , tile.zoom))
    val locations = tiles.map(x => tileLocation(x))
    val tempForLocations = locations.map(x => (x, predictTemperature(temperatures,x)))
    val colorForTemps = tempForLocations.map(x => (x, interpolateColor(colors,x._2)))
    val pxForLoc = colorForTemps.map(x => Pixel(x._2.red,x._2.green,x._2.blue, alpha = 176)).toArray
    Image(360, 180, pxForLoc)
  }
  //Web Mercator Projection
  def wmp(loc: Location, zoom: Int):(Double, Double) = {
    val x = (256/(2*math.Pi)) * math.pow(2,zoom) * math.toRadians(loc.lon + math.Pi)
    val y = (256/(2*math.Pi)) * math.pow(2,zoom) * (math.Pi - math.log(math.tan((math.Pi/4)+ math.toRadians(loc.lat/2))))
    (x, y)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    ???
  }

}
