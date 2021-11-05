using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BfsSearchConfig<TState>
    {
        public TState InitialState {get; set; }
        public Func<TState, IEnumerable<TState>> NextStates { get; set; }
        public Func<TState, bool> IsSuccessState = _ => false;
        public Func<TState, bool> CanLeadToSuccessState = _ => true;
    }
}