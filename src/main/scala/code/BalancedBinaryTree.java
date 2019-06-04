package code;

/**
 * @Author: xck
 * @File: BalancedBinaryTree
 * @Time: 16:50 2019/4/30
 */
public class BalancedBinaryTree {
    public boolean isBalanced(TreeNode root) {
        return depth(root) - depthMin(root) < 2;
    }

    public int depth(TreeNode root){
        if(root == null) return 0;
        return Math.max(depth(root.left),depth(root.right)) + 1;
    }

    public int depthMin(TreeNode root){
        if(root == null) return 0;
        return Math.min(depthMin(root.left),depthMin(root.right)) + 1;
    }

    public class TreeNode {
        int val;
        TreeNode left;
        TreeNode right;

        TreeNode(int x) {
            val = x;
        }
    }
}
