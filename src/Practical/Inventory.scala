object Inventory {
  var inventory1: Map[Int, (String, Int, Double)] = Map(
    101 -> ("Apple", 10, 15.50),
    102 -> ("Banana", 5, 20.00),
    103 -> ("Orange", 2, 45.75)
  )

  var inventory2: Map[Int, (String, Int, Double)] = Map(
    102 -> ("Banana", 3, 22.00),
    104 -> ("Pineapple", 7, 18.30)
  )

  def getAllProductNames(inventory: Map[Int, (String, Int, Double)]): List[String] = {
    inventory.values.map(_._1).toList
  }

  def calculateTotalValue(inventory: Map[Int, (String, Int, Double)]): Double = {
    inventory.values.map { case (_, qty, price) => qty * price }.sum
  }

  def isInventoryEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(inventory1: Map[Int, (String, Int, Double)],
                       inventory2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
    (inventory1.keySet ++ inventory2.keySet).map { key =>
      val item1 = inventory1.get(key)
      val item2 = inventory2.get(key)
      val mergedItem = (item1, item2) match {
        case (Some((name1, qty1, price1)), Some((name2, qty2, price2))) =>
          (name1, qty1 + qty2, price1 max price2)
        case (Some(item), None) => item
        case (None, Some(item)) => item
        case (None, None) => throw new IllegalStateException("Unexpected case")
      }
      key -> mergedItem
    }.toMap
  }

  def checkAndPrintProductDetails(inventory: Map[Int, (String, Int, Double)], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, qty, price)) => println(s"Product ID: $productId, Name: $name, Quantity: $qty, Price: $price")
      case None => println(s"Product with ID $productId not found.")
    }
  }


  def main(args: Array[String]): Unit = {
    println("Product Names in Inventory1: " + getAllProductNames(inventory1))

    println("Total Value of Inventory1: $" + calculateTotalValue(inventory1))

    println("Is Inventory1 empty? " + isInventoryEmpty(inventory1))

    val mergedInventory = mergeInventories(inventory1, inventory2)
    println("Merged Inventory: " + mergedInventory)

    checkAndPrintProductDetails(inventory1, 102)
  }
}

