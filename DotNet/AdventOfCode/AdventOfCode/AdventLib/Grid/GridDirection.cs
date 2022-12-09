namespace AdventOfCode.AdventLib.Grid
{
    public enum GridDirection
    {
        Up,
        Down,
        Left,
        Right
    }

    public enum GridInterpretation
    {
        /// <summary>
        /// Increasing Y is "down"
        /// </summary>
        Graphics,
        /// <summary>
        /// Increasing Y is "up
        /// </summary>
        Math
    }

    public static class GridDirectionExtensions
    {
        public static GridPoint AsUnitPoint(this GridDirection direction, GridInterpretation interpretation)
        {
            return direction switch
            {
                GridDirection.Left => new GridPoint(-1, 0),
                GridDirection.Right => new GridPoint(1, 0),
                GridDirection.Up => interpretation switch
                {
                    GridInterpretation.Graphics => new GridPoint(0, -1),
                    GridInterpretation.Math => new GridPoint(0, 1)
                },
                GridDirection.Down => interpretation switch
                {
                    GridInterpretation.Graphics => new GridPoint(0, 1),
                    GridInterpretation.Math => new GridPoint(0, -1)
                }
            };
        }
    }
}