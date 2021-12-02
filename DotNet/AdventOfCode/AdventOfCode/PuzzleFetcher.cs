using System;
using System.Net.Http;
using System.Text;

namespace AdventOfCode
{
    public class PuzzleFetcher
    {
        private readonly int _year;
        private readonly int _day;

        public PuzzleFetcher(int year, int day)
        {
            _year = year;
            _day = day;
        }
        private static string? SessionToken => Environment.GetEnvironmentVariable("SESSION_TOKEN");

        public byte[] FetchInput()
        {
            if (SessionToken == null)
            {
                return null;
            }
            try
            {
                var response = MakeRequest();
                var bytes = response.Content.ReadAsByteArrayAsync().Result;
                if (!DayHasStarted(bytes))
                {
                    Console.Out.WriteLine("Day has not started yet, creating empty file.");
                    return null;
                }
                Console.Out.WriteLine("Fetched puzzle input.");
                return bytes;
            }
            catch (Exception e)
            {
                Console.Out.WriteLine("Failed to fetch puzzle input, creating empty file");
                return null;
            }
        }

        private static bool DayHasStarted(byte[] bytes)
        {
            return !Encoding.UTF8.GetString(bytes).Contains("Please don't repeatedly request this endpoint before it unlocks!");
        }

        private HttpResponseMessage MakeRequest()
        {
            using var client = new HttpClient();
            var request = new HttpRequestMessage();
            request.Headers.Add("Cookie", $"session={SessionToken}");
            request.Method = HttpMethod.Get;
            request.RequestUri = new Uri($"https://adventofcode.com/{_year}/day/{_day}/input");

            var response = client.Send(request);
            return response;
        }
    }
}