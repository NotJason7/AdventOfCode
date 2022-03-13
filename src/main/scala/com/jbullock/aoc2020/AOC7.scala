// import scala.compiletime.ops.string
// val rulesString = """light red bags contain 1 bright white bag, 2 muted yellow bags.
// dark orange bags contain 3 bright white bags, 4 muted yellow bags.
// bright white bags contain 1 shiny gold bag.
// muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
// shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
// dark olive bags contain 3 faded blue bags, 4 dotted black bags.
// vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
// faded blue bags contain no other bags.
// dotted black bags contain no other bags."""

// val bagMap = Map(
//   "light red" -> List("bright white", "muted yellow"),
//   "dark orange" -> List("bright white", "muted yellow"),
//   "bright white" -> List("shiny gold"),
//   "muted yellow" -> List("shiny gold", "faded blue"),
//   "shiny gold" -> List("dark olive", "vibrant plum"),
//   "dark olive" -> List("faded blue", "dotted black"),
//   "vibrant plum" -> List("faded blue", "dotted black"),
//   "faded blue" -> List(),
//   "dotted black" -> List()
// )

// def findBagList(targetBagList: List[String], bagMap: Map[String,List[String]]): List[String] =

//   // bagMap.toList.size match
//   //   case 0 => targetBagList
//   //   case _ =>
//   val nextLevelBagList = bagMap.filter((t) => !targetBagList.toSet.intersect(t._2.toSet).isEmpty).keys.toList ++ targetBagList
//   println(nextLevelBagList)
//   val nextLevelBagMap = bagMap.filter(_.nonEmpty).filter((t) => !nextLevelBagList.contains(t._1)).toMap
//   println(nextLevelBagMap)
//   findBagList(nextLevelBagList, nextLevelBagMap)
//   // val bagsContainingTargetBags = bagMap map {
//   //   case (key, value) if value.toSet.intersect(targetBagList.toSet).size > 0 => key
//   //   case (_) => ""
//   // }.filter(_.nonEmpty)

//   // bagsContainingTargetBag.size match
//   //   case 0 => confirmedBagList
//   //   case _ =>
//   //     val totalBagList = confirmedBagList ++ bagsContainingTargetBag.filter(_.nonEmpty).toList
//   //     val filteredBagMap = bagMap.view.filterKeys(k => !totalBagList.contains(k))
//   //     findBagList()

//   // println(newGoodBagsList)
//   // newGoodBagsList
//   // List()

// @main
// def runScript() =
//   val targetBag = List("shiny gold")
//   val totalBagList = findBagList(targetBag, bagMap)
//   println(totalBagList)
