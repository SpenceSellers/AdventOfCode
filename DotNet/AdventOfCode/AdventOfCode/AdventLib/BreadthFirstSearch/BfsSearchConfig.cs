using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BfsSearchConfig<TState>
    {
        public TState InitialState {get; set; }
        public Func<TState, IEnumerable<TState>> NextStates { get; set; }
        public Func<TState, bool> IsSuccessState = _ => false;
        // TODO do we need this?
        public Func<TState, bool> CanLeadToSuccessState = _ => true;

        public Func<TState, object> SeenKey = state => state;
    }
}