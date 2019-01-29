import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object FileOps {
  private val filename = "Values.txt"
  private val separator = "|~|"

  def saveDataToFile(currency: String, coins:List[LocalCoinInfo]): Boolean = {
      for (i <- 1 to 5) {
        if (
          try {
            val file = new File(filename)
            val writer = new PrintWriter(file)

            writer.println(currency)
            for (coin <- coins) {
              writer.println(coin.id + separator + coin.numToTrack)
            }
            writer.close()
            true
          } catch {
            case error: Throwable => {
              false
            }
          }
        ) {
          return true
        }
      }
      println("File save failed.")
    false
  }

  def readDataFromFile(): Option[(String, mutable.HashMap[String, Double])] = {
    var currency = ""
    val coins = new mutable.HashMap[String, Double]()
    for (i <- 1 to 5) {
          val bufferedSource = Source.fromFile(filename)
          var firstTime = true
          for (line <- bufferedSource.getLines) {
            if (firstTime) {
              if (!line.matches("[a-zA-Z]{3}")) {
                return None
              }
              currency = line
              firstTime = false
            } else {
              //id|num
              val str = line.split(CMCDisplay.separator.head)
              Try(coins.put(str(0), str(1).toDouble))
            }
          }
          bufferedSource.close()
        }
    if(coins.isEmpty) None
    else Option((currency, coins))
    }
}