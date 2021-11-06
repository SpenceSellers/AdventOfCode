using System;
using System.Collections.Generic;

namespace AdventOfCode.AdventLib.BreadthFirstSearch
{
    public class BfsSearchConfig<TState>
    {
        public TState InitialState {get; set; }
        public Func<TState, IEnumerable<TState>> NextStates { get; set; }
        public Func<TState, bool> IsSuccessState = _ => false;

        public Func<TState, object> SeenKey = state => state;
    }

    // TODO maybe prefer this:
    // public interface ISearchConfiguration<TState>
    // {
    //     public TState InitialState { get; }
    //     public IEnumerable<TState> NextStates(TState state);
    //     public bool IsSuccessState(TState state);
    //     public object SeenKey(TState state);
    // }
}