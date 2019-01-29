import scala.util.Try

class CMCDriver {
  def main(args:Array[String]): Unit ={
    val localCoins = FileOps.readDataFromFile()
    if(localCoins.isDefined){
      val coinsFromApi = RESTOps.getCoinInfo(currency = localCoins.get._1)
      val coinLookupTable = localCoins.get._2
      val coins = coinsFromApi.flatMap {
        coin =>
          val id = coin.id
          if(coinLookupTable.contains(id)) {
            val name = coin.name
            val value = coin.value
            val symbol = coin.symbol
            val numToTrack = coinLookupTable.get(id).getOrElse(1)
            val totalValue = numToTrack * value.toDouble
            Some(LocalCoinInfo(name, id, value, symbol, numToTrack, totalValue))
          }else None
      }
      showLocalInfo(coins)
    }else{
      showApiInfo(RESTOps.getCoinInfo())
    }
  }

  def showLocalInfo(coins: List[LocalCoinInfo]) ={

  }

  def showApiInfo(coins: List[CoinApiInfo]) = {

  }
}

case class CoinApiInfo(name: String, id: String, value: String, symbol: String)
case class LocalCoinInfo(name:String, id:String, value:String, symbol:String, numToTrack:Double, totalValue: Double)