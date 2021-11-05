using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BfsSearchConfig<TState>
    {
        public TState InitialState {get; set; }
        public Func<BfsNode<TState>, IEnumerable<BfsNode<TState>>> NextStates { get; set; }
        public Func<BfsNode<TState>, bool> IsSuccessState = _ => false;
        public Func<BfsNode<TState>, bool> CanLeadToSuccessState = _ => true;
    }
}