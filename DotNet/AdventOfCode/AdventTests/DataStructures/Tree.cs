using System.Collections.Generic;
using System.Text;

namespace AdventTests.DataStructures;

public class Tree<TNodeData>
{
    public TreeNode<TNodeData> Root = null;
}

public class TreeNode<TNodeData>
{
    public List<TreeNode<TNodeData>> Children = new();
    public TNodeData Data;
}