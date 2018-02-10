import java.io.{File, PrintWriter}

import CMCDisplay.{currencies, retriever}
import org.json.{JSONArray, JSONObject}

import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}


case class Coin(ticker: String, var name: String = "", var value: Double = -1,
                var numOwned: Double = 0, var error: Boolean = false)

case class Header(name: String, prefix: String = "", postfix: String = CMCDisplay.separator, lengthModifier: Int = 0)

/*
* Class to retrieve values from CoinMarketCap.com, based on several user preferences.
*
* @author Austin Leek
* @Date created 01/11/2018
*/
class CMCJSONRetriever(var useSelectedTickers: Boolean = false,
                       private var selectedTickers: ArrayBuffer[Coin] = new ArrayBuffer[Coin](),
                       private var selectedCurrency: String = "USD") {


  val allValidCurrencies: Array[String] = Array("AUD", "BRL", "CAD", "CHF",
    "CLP", "CNY", "CZK", "DKK", "EUR",
    "GBP", "HKD", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PKR", "PLN", "RUB",
    "SEK", "SGD", "THB", "TRY", "TWD", "ZAR", "USD")
  val errorMessagePrefix = "There was an error retrieving ticker '"
  val errorMessagePostfix = "'."
  val timeoutErrorMessage = "There was an error retrieving data from the server. Please try again later."
  private val _allTickersURLPrefix = "https://api.coinmarketcap.com/v1/ticker/"
  private val _currencyPostfix = "?convert="

  /*
  * Get the selected tickers
  *
  * @return selectedTickers - the tickers currently selected
  */
  def getSelectedTickers = {
    selectedTickers
  }

  /*
  * Set the selected tickers
  *
  * @param newValue - the tickers to be selected
  */
  def setSelectedTickers_=(newValue: ArrayBuffer[Coin]): Unit = {
    selectedTickers = newValue
  }

  /*
  * Set the user setting to use the selected tickers or get all tickers
  *
  * @param newValue - to select specific tickers input true
  */
  def setUseSelectedTickers(newValue: Boolean): Unit = {
    useSelectedTickers = newValue
  }

  /*
  * Set the selected currency
  *
  * @param selectedTickers - the currency to use.
  *
  * @return true if the currency switch was successful, false if the new currency is invalid.
  */
  def setSelectedCurrency(newValue: String): Boolean = {
    val temp = newValue.toUpperCase()
    if (temp == "USD") {
      return true
    }
    for (currency: String <- allValidCurrencies) {
      if (currency == temp) {
        selectedCurrency = temp
        return true
      }
    }
    false
  }

  /*
  * method to apply the value of selected coins to the array of all coins.
  *
  * @return the array of coins
  */
  def getJSONValues: ArrayBuffer[Coin] = {
    if (useSelectedTickers) {
      getSpecificTickers()
    } else {
      toJson(get())
    }
    selectedTickers
  }

  /*
  * method to put the top 100 coins value into the selected tickers array.
  */
  def toJson(json: String) = {
    val jsonArray = new JSONArray(json)
    for (i <- 0 until jsonArray.length()) {
      val json = new JSONObject(jsonArray.get(i).toString)
      selectedTickers += new Coin(json.getString("id"), name = json.getString("name"),
        value = json.getDouble(s"price_${selectedCurrency.toLowerCase()}"))
    }
  }

  /*
  * method to get the JSON String from all selected tickers.
  *
  * @return the JSON of the selected tickers
  */
  private def getSpecificTickers() = {
    for (ticker <- selectedTickers) {
      try {
        val json = new JSONObject(new JSONArray(get(url = getSpecificURL(ticker.ticker))).get(0).toString())
        ticker.value = json.getDouble(s"price_${selectedCurrency.toLowerCase}")
        ticker.name = json.getString("name")
      } catch {
        case ioe: java.io.IOException => {
          ticker.error = true
          ticker.name = errorMessagePrefix + ticker.ticker + errorMessagePostfix
        }
        case ste: java.net.SocketTimeoutException => {
          ticker.error = true
          ticker.name = timeoutErrorMessage
        }
      }
    }
  }

  /*
  * method to get the selected URL based on input ticker symbol.
  *
  * @param ticker - the ticker symbol to retrieve
  *
  * @return the string form of a URL based on user preferences for a specific ticker.
  */
  private def getSpecificURL(ticker: String) = {
    _allTickersURLPrefix.concat(ticker + "/")
      .concat(if (selectedCurrency != "USD") {
        _currencyPostfix + selectedCurrency
      } else {
        ""
      })
  }

  /*
  * method to get the JSON values from the API.
  *
  * @return the JSON returned by the selected URL. Returns top 100 currencies by default.
  */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  private def get(url: String = getURL(),
                  connectTimeout: Int = 5000,
                  readTimeout: Int = 5000,
                  requestMethod: String = "GET") = {
    import java.net.{HttpURLConnection, URL}
    val connection = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)
    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) {
      inputStream.close()
    }
    content
  }

  /*
  * Get the selected currency
  *
  * @return selectedCurrency - the currency currently selected
  */
  def getSelectedCurrency = {
    selectedCurrency
  }

  /*
  * method to get the URL used for all tickers
  *
  * @return the string form of a URL based on user preferences
  */
  private def getURL = {
    () => {
      _allTickersURLPrefix.concat(if (selectedCurrency != "USD") {
        _currencyPostfix + selectedCurrency
      } else {
        ""
      })
    }
  }
}

object CMCDisplay {

  val separator = "|"
  val headerChar = "*"
  val headerFix = {
    var temp = ""
    for (i <- 1 to numOfPaddingChars) {
      temp += headerChar
    }
    temp
  }
  private val numOfPaddingChars = 2
  private val outputHeaders = Array[Header](new Header("Coin", prefix = headerFix + headerChar, lengthModifier = -1),
    new Header("Value"), new Header("Coins"), new Header("Balance"),
    new Header("Total", postfix = headerChar + headerFix + "\n", lengthModifier = -1))
  private val outputHeaderSpacer: ArrayBuffer[Header] = {
    var temp = new ArrayBuffer[Header]
    for (i <- 1 to outputHeaders.length) {
      temp += new Header("", prefix = if (i == 1) {
        headerFix
      } else {
        ""
      },
        postfix = if (i == outputHeaders.length) {
          headerFix + "\n"
        } else {
          separator
        })
    }
    temp
  }
  var currencies = new ArrayBuffer[Coin]()
  var retriever = new CMCJSONRetriever()
  private var runningBalance = 0.0

  def main(args: Array[String]): Unit = {
    //    println("Starting...")
    getValuesFromUser
    if (currencies.length > 0) {
      retriever.setSelectedTickers_=(currencies)
      retriever.setUseSelectedTickers(true)
    }
    formatAndOutput(retriever.getJSONValues)
    FileOps.saveDataToFile
    //    println("Finished. Press return to quit...")
    StdIn.readLine
  }

  def getValuesFromUser: Unit = {
    var input = ""

    if (FileOps.readDataFromFile) {
      return ()
    } //attempt to read file

    println("Enter 'quit' at any time to end execution")
    while (!retriever.setSelectedCurrency(input)) {
      println("Enter the currency you would like to use. For a list of currencies, enter 'list'." +
        "Entering 'none' defaults to USD.")

      input = StdIn.readLine()
      if (input == "list") {
        println(retriever.allValidCurrencies.mkString("|"))
      } else if (input == "quit") {
        return ()
      } else if (input == "none") {
        input = "USD"
      }
    }
    println("Enter the coins you would like to track. " +
      "\nEnter 'none' to track top 100 coin prices.\nEnter 'end' to stop entering coins.")
    while (input != "quit") {
      println("Enter a coin to track: ")
      input = StdIn.readLine()
      if (input == "none") {
        retriever.setUseSelectedTickers(false)
        return ()
      } else if (input == "end") {
        input = "quit"
      } else if (input == "quit") {
        return ()
      } else {
        currencies += new Coin(input)
      }
    }

    if (currencies.length != 0) {
      println("Now enter the number of each currency you would like to calculate.")
      for (currency <- currencies) {
        println(s"Enter the number of ${currency} you own: ")
        input = StdIn.readLine()
        if (input == "quit") {
          return ()
        } else {
          currency.numOwned = input.toDouble
        }
      }
    }
  }

  def formatAndOutput(input: ArrayBuffer[Coin]) = {
    //    print("Printing.")
    //    Thread.sleep(500)
    //    print(".")
    //    Thread.sleep(500)
    //    print(".")
    val length = outputHeaders.length * StringOps.lengthOfField + numOfPaddingChars * 4
    //    println("\n\n" + StringOps.getHeader(length))
    println(StringOps.getHeader(length))
    for (header <- outputHeaders) {
      print(StringOps.formatHeaderString(header))
    }
    for (header <- outputHeaderSpacer) {
      print(StringOps.formatHeaderString(header))
    }
    input.foreach(printCoin)
    //    println(StringOps.getHeader(length) + "\n\n")
    println(StringOps.getHeader(length))
  }

  def printCoin(coin: Coin): Unit = {
    var output = ""
    if (coin.error) {
      output = StringOps.ensureCorrectSizeCenterJustified(coin.name,
        StringOps.lengthOfField * outputHeaders.length + outputHeaders.length - 1)
    } else {
      val totalValue = coin.numOwned * coin.value
      runningBalance += totalValue

      output = StringOps.formatString(coin.name) + StringOps.formatNumber(coin.value) +
        StringOps.formatNumber(coin.numOwned, false) +
        StringOps.formatNumber(totalValue) + StringOps.formatNumber(runningBalance, isEnd = true)
    }
    print(headerChar)
    for (i <- 2 to numOfPaddingChars) {
      print(" ")
    }
    print(output)
    for (i <- 2 to numOfPaddingChars) {
      print(" ")
    }
    println(headerChar)
  }
}

object StringOps {
  val lengthOfField = 14

  def getHeader(numOfTimes: Int, str: String = CMCDisplay.headerChar): String = {
    var temp = ""
    for (i <- 1 to numOfTimes) {
      temp += str
    }
    temp
  }

  def formatString(value: String): String = {
    ensureCorrectSizeLeftJustified(value) + "|"
  }

  def ensureCorrectSizeLeftJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      var temp = str
      while (temp.length < size) {
        temp = temp + " "
      }
      temp
    } else if (str.length() > size) {
      removeLengthFromString(str, str.length - size)
    } else {
      str
    }
  }

  def removeLengthFromString(str: String, i: Int): String = {
    str.substring(0, str.length() - i)
  }

  def formatHeaderString(value: Header): String = {
    value.prefix + ensureCorrectSizeCenterJustified(value.name, size = lengthOfField + value.lengthModifier) + value.postfix
  }

  def formatNumber(value: Double, isCurrency: Boolean = true, isEnd: Boolean = false): String = {
    val str = (math rint (value * 100)).toString
    val middleIndex = if (str.length > 3) {
      str.length - 4
    } else {
      1
    }

    var outputStr = str.substring(0, middleIndex) + "." + str.substring(middleIndex, str.length - 2)

    outputStr += (if (outputStr.matches("""[\$0-9]*\.[0-9]""")) {
      "0"
    } else {
      if (outputStr.endsWith(".")) {
        "00"
      } else {
        ""
      }
    })

    if (isCurrency) {
      outputStr = "$" + outputStr
    }
    ensureCorrectSizeRightJustified(outputStr) + (if (!isEnd) {
      "|"
    } else {
      ""
    })
  }

  def ensureCorrectSizeRightJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      addLengthToBackOfString(str, size - str.length())
    } else {
      str + " "
    }
  }

  def ensureCorrectSizeCenterJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      addLengthToFrontAndBackOfString(str, size - str.length)
    } else if (str.length() > size) {
      removeLengthFromString(str, str.length - size)
    } else {
      str
    }
  }

  def addLengthToFrontOfString(str: String, i: Int): String = {
    if (i == 1) {
      " " + str
    } else {
      addLengthToFrontOfString(" " + str, i - 1)
    }
  }

  def addLengthToBackOfString(str: String, i: Int): String = {
    if (i == 1) {
      str + " "
    } else {
      addLengthToFrontOfString(str + " ", i - 1)
    }
  }

  def addLengthToFrontAndBackOfString(str: String, i: Int): String = {
    if (i == 1) {
      " " + str
    } else {
      addLengthToBackAndFrontOfString(" " + str, i - 1)
    }
  }

  def addLengthToBackAndFrontOfString(str: String, i: Int): String = {
    if (i == 1) {
      str + " "
    } else {
      addLengthToFrontAndBackOfString(str + " ", i - 1)
    }
  }
}

object FileOps {
  private val filename = "Values.txt"

  def saveDataToFile(): Unit = {
    if (retriever.useSelectedTickers) {
      for (i <- 1 to 5) {
        if (
          try {
            val file = new File(filename)
            val writer = new PrintWriter(file)

            writer.println(retriever.getSelectedCurrency)
            var iter = 0
            for (coin <- currencies) {
              writer.println(coin.ticker + CMCDisplay.separator + coin.numOwned)
              iter += 1
            }
            writer.close()
            true
          } catch {
            case error: Throwable => {
              false
            }
          }
        ) {
          return ()
        }
      }
      println("File read failed.")
    }
  }

  def readDataFromFile(): Boolean = {
    for (i <- 1 to 5) {
      if (
        try {
          val bufferedSource = Source.fromFile(filename)
          var firstTime = true
          for (line <- bufferedSource.getLines) {
            if (firstTime) {
              if (!line.matches("[a-zA-Z]{3}")) {
                return false
              }
              retriever.setSelectedCurrency(line)
              firstTime = false
            } else {
              val str = line.split(CMCDisplay.separator.head)
              currencies += Coin(str(0), numOwned = if (str.length > 1) {
                str(1).toDouble
              } else {
                0
              })
            }
          }
          bufferedSource.close()
          !firstTime
        } catch {
          case error: Throwable => false
        }) {
        return true
      }
    }
    println("Data read failed.\n")
    false
  }
}