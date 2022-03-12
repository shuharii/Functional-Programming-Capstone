package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val p = 2.0
    temperatures.map(a => a._2/math.pow(gcd(a._1, location), p)).sum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    //value == points.temp return color
    //otherwise interpolate the color with upper and lower bound
    def interpolate(a : (Temperature, Color), b: (Temperature, Color), value: Temperature): Color = {
      if(a._1 == value) a._2
      else if(b._1 == value) b._2
      else Color(lerp(a._2.red, b._2.red, value),lerp(a._2.green, b._2.green, value),lerp(a._2.blue, b._2.blue, value))
    }
    def lerp(v0:Int, v1:Int, t:Temperature): Int = ((1-t) * v0 + t * v1).toInt
    val (upper, lower) = points.toList.sortBy(_._1).partition(_._1 > value)
    interpolate(upper.head,lower.head, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    //create locations
    //find all temp for loc
    //find all color for temp
    //find all pixel for loc
    //create image with all pixel.
    val locations = (0 to 360 * 180).map(x => Location(90-(x/360), (x%360)-180))
    val tempForLocations = locations.map(x => (x, predictTemperature(temperatures,x)))
    val colorForTemps = tempForLocations.map(x => (x, interpolateColor(colors,x._2)))
    val pxForLoc = colorForTemps.map(x => Pixel(x._2.red,x._2.green,x._2.blue, alpha = 255)).toArray
    Image(360, 180, pxForLoc)
  }

  def gcd(loca: Location, locb: Location): Double = {
    def isAntipode(location: Location, location1: Location): Boolean = {
      val tempLat = if(location.lat > 0) (location.lat + 180) % 180 else (location.lat - 180) % 180
      val tempLon = if(location.lon > 0) (location.lon - 180) % 180 else (location.lon + 180) % 180
      (tempLat == location1.lat) && (tempLon == location1.lon)
    }
    val delta: Double = {
      if(loca == locb) 0
      else if (isAntipode(loca, locb)) math.Pi
      else {
        val cos1 = math.sqrt(1 - math.pow(math.sin(math.toRadians(loca.lat)),2))
        val cos2 = math.sqrt(1 - math.pow(math.sin(math.toRadians(locb.lat)),2))
        val tempA = math.sin(math.toRadians(loca.lat)) * math.sin(math.toRadians(locb.lat))
        val tempB = cos1 * cos2 * math.cos(math.abs(math.toRadians(loca.lon) - math.toRadians(locb.lon)))
        math.acos(tempA + tempB)
      }
    }
    delta*6371
  }
}

