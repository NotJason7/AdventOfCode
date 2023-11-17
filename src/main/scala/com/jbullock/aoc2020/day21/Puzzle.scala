package com.jbullock.aoc2020.day21

import scala.annotation.tailrec
import scala.collection.MapView

@main def solvePuzzle(): Unit =
  val input = scala.io.Source.fromResource("aoc/2020/Day21/Input.txt").getLines.toVector
  val foods = input.map { case s"$ingredientString (contains $allergenString)" =>
    val ingredients = ingredientString.split(" ").toSet
    val allergens   = allergenString.replace(", ", " ").split(" ").toSet
    Food(ingredients, allergens)
  }
  val allIngredients = foods.flatMap(_.ingredients).toSet
  val possibleIngredientAllergens = allIngredients
    .map(ingredient =>
      val possibleAllergens = foods
        .filter(_.ingredients.contains(ingredient))
        .flatMap(_.allergens)
        .filterNot(allergen =>
          foods.exists(food => !food.ingredients.contains(ingredient) && food.allergens.contains(allergen))
        )
      ingredient -> possibleAllergens
    )
    .toMap
  val noAllergens = possibleIngredientAllergens.filter(_._2.isEmpty).keys.toVector
  val part1       = foods.flatMap(_.ingredients.toVector).count(ingredient => noAllergens.contains(ingredient))
  println(s"Part 1: $part1")
  val allergens           = possibleIngredientAllergens.filter(_._2.nonEmpty)
  val identifiedAllergens = findAllergens(allergens)
  val part2               = identifiedAllergens.toVector.sortWith(_._2 < _._2).map(_._1).mkString(",")
  println(s"Part 2: $part2")

def findAllergens(possibleAllergens: Map[Ingredient, Vector[Allergen]]): Map[Ingredient, Allergen] =
  @tailrec
  def loop(toSearch: Map[Ingredient, Set[Allergen]], identified: Map[Ingredient, Allergen]): Map[Ingredient, Allergen] =
    if toSearch.isEmpty then identified
    else
      val (newlyIdentified, stillToSearch) = toSearch.partition { case (_, allergens) => allergens.size == 1 }
      val newAllergens                     = newlyIdentified.flatMap(_._2).toVector
      val updatedToSearch = stillToSearch.view.mapValues(_.filterNot(allergen => newAllergens.contains(allergen))).toMap
      val updatedIdentified             = identified ++ newlyIdentified.view.mapValues(_.head).toMap
      loop(updatedToSearch, updatedIdentified)
  val start = possibleAllergens.view.mapValues(_.toSet).toMap
  loop(start, Map.empty[Ingredient, Allergen])

type Ingredient = String
type Allergen   = String
case class Food(ingredients: Set[Ingredient], allergens: Set[Allergen])
