using System.Collections;
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
            var allIngredients = AllIngredients(foods);
            var knownForSures = LearnPossibleAllergens(foods);

            var allergyIngredients = knownForSures.Values.SelectMany(v => v).ToHashSet();
            var aaa = allIngredients.ToHashSet();
            aaa.ExceptWith(allergyIngredients);

            var total = foods.Select(food => food.Ingredients.Count(i => aaa.Contains(i))).Sum();
            return total.ToString();
        }

        private static Dictionary<string, HashSet<string>> LearnPossibleAllergens(List<Food> foods)
        {
            var allAllergens = foods.SelectMany(food => food.Allergens).ToHashSet();

            var knownForSures = allAllergens.ToDictionary(allergen => allergen, allergen => Options(allergen, foods).ToHashSet());
            return knownForSures;
        }

        public override string PartTwo(string[] input)
        {
            var foods = input.Select(ParseFood).ToList();
            var knownForSures = LearnPossibleAllergens(foods);
            SolveForSingles(knownForSures);

            var knownSingles = knownForSures.ToDictionary(kv => kv.Key, kv => kv.Value.First());

            return string.Join(',', knownSingles.Keys.OrderBy(x => x).Select(k => knownSingles[k]));
        }

        private static void SolveForSingles(Dictionary<string, HashSet<string>> knownForSures)
        {
            var visited = new HashSet<string>();
            while (true)
            {
                var single = knownForSures.Where(kv => !visited.Contains(kv.Key) && kv.Value.Count == 1).Select(kv => kv.Key)
                    .FirstOrDefault();
                if (single == null)
                {
                    break;
                }

                visited.Add(single);

                var ingredient = knownForSures[single].First();
                foreach (var allergen in knownForSures.Keys)
                {
                    if (allergen == single) continue;
                    knownForSures[allergen].Remove(ingredient);
                }
            }
        }

        private static IEnumerable<string> Options(string allergen, IEnumerable<Food> foods)
        {
            var ingredientSets = foods.Where(food => food.Allergens.Contains(allergen)).Select(food => food.Ingredients)
                .ToList();

            var common = ingredientSets.First().ToHashSet();
            foreach (var ingredients in ingredientSets.Skip(1))
            {
                common.IntersectWith(ingredients);
            }

            return common;
        }

        private static List<Food> Strike(List<Food> foods, string ingredient, string allergen)
        {
            return foods.Select(food => food with {
                Ingredients = food.Ingredients.Where(i => i != ingredient).ToHashSet(), 
                Allergens = food.Allergens.Where(a => a != allergen).ToHashSet()}).ToList();
        }

        private HashSet<string> AllIngredients(IEnumerable<Food> foods) =>
            foods.SelectMany(food => food.Ingredients).ToHashSet();

        private Food ParseFood(string line)
        {
            var r = new Regex(@"^(.*) \(contains (.*)\)$");
            var match = r.Match(line);
            var ingredients = match.Groups[1].Value.Split(' ');
            var allergens = match.Groups[2].Value.Split(',').Select(s => s.Trim());
            return new Food(ingredients.ToHashSet(), allergens.ToHashSet());
        }

        private record Food(HashSet<string> Ingredients, HashSet<string> Allergens);
    }
}