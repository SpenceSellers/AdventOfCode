using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Days
{
    public class Advent202021 : Problem
    {
        public override string PartOne(string[] input)
        {
            var foods = input.Select(ParseFood).ToList();

            var knownAllergens = new Dictionary<string, string>();

            while (true)
            {
                var knownPairings = KnownPairings(foods).ToList();

                if (!knownPairings.Any())
                {
                    break;
                }

                foreach (var (ingredient, allergen) in knownPairings)
                {
                    knownAllergens.TryAdd(ingredient, allergen);
                    foreach (var food in foods)
                    {
                        food.Allergens.Remove(allergen);
                        food.Ingredients.Remove(ingredient);
                    }
                }
            }
            
            var noAllergens = AllIngredients(foods);
            noAllergens.ExceptWith(knownAllergens.Keys);

            return foods.Select(food => food.Ingredients.Count).Sum().ToString();
        }

        private static IEnumerable<(string, string)> KnownPairings(List<Food> foods)
        {
            foreach (var fooda in foods)
            {
                if (fooda.Allergens.Count == 1 && fooda.Ingredients.Count == 1)
                {
                    Console.Out.WriteLine($"FOUND SINGLE {fooda.Ingredients.First()}- {fooda.Allergens.First()}");
                    yield return (fooda.Ingredients.First(), fooda.Allergens.First());
                }
                
                foreach (var foodb in foods)
                {
                    if (fooda == foodb) continue;
                    var (commonIngredients, commonAllergens) = fooda.Commonality(foodb);
                    if (commonAllergens.Count == 1 && commonIngredients.Count == 1)
                    {
                        Console.Out.WriteLine($"FOUND PAIR {commonIngredients.First()} - {commonAllergens.FirstOrDefault()}");
                        yield return (commonIngredients.First(), commonAllergens.First());
                    }
                }
            }
        }

        private HashSet<string> AllIngredients(IEnumerable<Food> foods) => foods.SelectMany(food => food.Ingredients).ToHashSet();

        public override string PartTwo(string[] input)
        {
            throw new System.NotImplementedException();
        }

        private Food ParseFood(string line)
        {
            var r = new Regex(@"^(.*) \(contains (.*)\)$");
            var match = r.Match(line);
            var ingredients = match.Groups[1].Value.Split(' ');
            var allergens = match.Groups[2].Value.Split(',').Select(s => s.Trim());
            return new Food(ingredients.ToHashSet(), allergens.ToHashSet());
        }

        private record Food(HashSet<string> Ingredients, HashSet<string> Allergens)
        {
            public Food Commonality(Food other)
            {
                var ingredients = Ingredients.ToHashSet();
                ingredients.IntersectWith(other.Ingredients);

                var allergens = Allergens.ToHashSet();
                allergens.IntersectWith(other.Allergens);
                return new Food(ingredients, allergens);
            }
        }
    }
}