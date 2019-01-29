import org.apache.commons.lang3.StringUtils
import org.json.{JSONArray, JSONObject}

import scala.collection.mutable.ListBuffer

object RESTOps {

  val URL_PREFIX = "https://apicoinmarketcap.com/v1/ticker/"
  val LIMIT_PREFIX = "?limit="
  val CURRENCY_PREFIX = "&convert="
  val VALID_CURRENCIES: Array[String] = Array("AUD", "BRL", "CAD", "CHF",
    "CLP", "CNY", "CZK", "DKK", "EUR",
    "GBP", "HKD", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PKR", "PLN", "RUB",
    "SEK", "SGD", "THB", "TRY", "TWD", "ZAR", "USD")

  def getCoinInfo(limit: Int = 0, currency:String = "usd"):List[CoinApiInfo] = {
    var url = URL_PREFIX + LIMIT_PREFIX + limit.toString
    if(VALID_CURRENCIES.contains(currency)) url += CURRENCY_PREFIX + currency
    val coins = new ListBuffer[CoinApiInfo]()

    val jsonCoinInfo = new JSONArray(executeRestCall(url))
    for(i <- 0 until jsonCoinInfo.length()){
      val coinJson = new JSONObject(jsonCoinInfo.get(i).toString)
      val id = coinJson.getString("id")
      val value = coinJson.getString(s"price_$currency")
      val name = coinJson.getString("name")
      val symbol = coinJson.getString("symbol")
      coins += CoinApiInfo(name, id, value, symbol)
    }

    coins.toList
  }

  def executeRestCall(url:String = URL_PREFIX) = {

  }
}
