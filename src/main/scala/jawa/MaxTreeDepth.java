package jawa;

import java.util.*;

public class MaxTreeDepth {
    static int f1(TreeNode root) {
        if(root == null) return 0;
        return 1 + Math.max(f1(root.left),f1(root.right));
    }

    static int f2(TreeNode root){
        Stack<TreeNode> nodes = new Stack<>();
        Stack<Integer> values = new Stack<>();
        nodes.push(root);
        values.push(root.val);
        int max = 0;
        while(!nodes.empty()){
            TreeNode node = nodes.pop();
            max = Math.max(max,node.val)  + 1;
            if(node.left != null)
                nodes.push(node.left);
            if(node.right != null)
                nodes.push(node.right);
        }
        return max;
    }
    static int f3(TreeNode root){
        Queue<TreeNode> queue = new LinkedList<>();
        queue.offer(root);
        int count = 1;
        while(!queue.isEmpty()){
            int size = queue.size();
            while(size-- > 0){
                TreeNode node = queue.poll();
                if(node.left != null)
                    queue.offer(node.left);
                if(node.right != null)
                    queue.offer(node.right);
            }
            count++;
        }
        return count;
    }

    static List<List<Integer>> f4(TreeNode root){
        List<List<Integer>> res = new ArrayList<>();
        if(root == null) return res;
        Queue<TreeNode> queue = new LinkedList<>();
        queue.offer(root);
        while(!queue.isEmpty()){
            List<Integer> list = new ArrayList<>();
            int size = queue.size();
            while(size-- > 0){
                TreeNode node = queue.poll();
                list.add(node.val);
                if(node.left != null)
                    queue.offer(node.left);
                if(node.right != null)
                    queue.offer(node.right);
            }
            res.add(list);
        }

        return res;
    }

    static class TreeNode {
        int val;
        TreeNode left;
        TreeNode right;

        TreeNode(int x) {
            val = x;
        }
    }

    public ArrayList<Integer> PrintFromTopToBottom(TreeNode root) {
        ArrayList<Integer> list = new ArrayList<>();
        Queue<TreeNode> queue = new LinkedList<>();
        queue.offer(root);
        while(!queue.isEmpty()){
            TreeNode tree = queue.poll();
            list.add(tree.val);
            if(tree.left != null)
                queue.offer(tree.left);
            if(tree.right != null)
                queue.offer(tree.right);
        }
        return list;
    }


    public static String minWindow(String s, String t) {
        if(s.length() < t.length()) return "";
        Map<Character,Integer> map = new HashMap<>();
        for(char c:t.toCharArray()){
            Integer count = map.get(c);
            if(count == null)
                map.put(c,1);
            else
                map.put(c,count + 1);
        }

        int slow = 0;
        int fast = 0;
        int matchCount = 0;
        int minLen = 100000;
        int index = 0;
        for(;fast < s.length();fast++){
            Character c = s.charAt(fast);
            Integer cCount = map.get(c);
            if(cCount == null)
                continue;
            map.put(c,cCount - 1);
            if(cCount == 1)
                matchCount++;
            while(matchCount == map.size()){
                if(fast - slow + 1 < minLen){
                    minLen = fast - slow + 1;
                    index = slow;
                }
                Character cc = s.charAt(slow++);
                Integer ccCount = map.get(cc);
                if(ccCount == null)
                    continue;
                map.put(cc,ccCount + 1);
                if(ccCount == 0){
                    matchCount--;
                }
            }
        }
        return minLen == Integer.MAX_VALUE ? "" : s.substring(index,index + minLen);
    }
    public static int f2(int target) {
     if(target == 1) return 1;
        if(target == 2) return 2;
        int result = 0;
        int a = 1;
        int b = 2;
        for(int i = 3;i <= target ;i++){
            result = a + b;
            a = b;
           b = result;
        }
        return result;
    }

    public static int f1(int target){

        int[] dp = new int[target +1];
        dp[1] = 1;
        dp[2] = 2;
        dp[0] = 0;
        for(int i = 3;i < target + 1;i++){
            dp[i] = dp[i - 1] +  dp[i - 2];
        }
        return dp[target];
    }

    public static ArrayList<Integer> spiralOrder(int[][] matrix) {
        ArrayList<Integer> result = new ArrayList<>();
        if(matrix == null || matrix.length == 0 || matrix[0].length == 0) return result;
        int m = matrix.length;
        int n = matrix[0].length;
        int count = 0;
        int rowStart = 0;
        int rowEnd = m - 1;
        int colStart = 0;
        int colEnd = n - 1;
        while(count < m * n){
            for(int i = colStart;i <= colEnd;i++){
                result.add(matrix[rowStart][i]);
                count++;
            }
            rowStart++;
            for(int i = rowStart;i <= rowEnd;i++){
                result.add(matrix[i][colEnd]);
                count++;
            }
            colEnd--;
            for(int i = colEnd;i >= colStart;i--){
                result.add(matrix[rowEnd][i]);
                count++;
            }
            rowEnd--;
            for(int i = rowEnd;i >= rowStart;i--){
                result.add(matrix[i][colStart]);
                count++;
            }
            colStart++;
        }
        return  result;
    }

    public static void main(String[] args) {
        int[][] a = new int[][]{
                {1,2,3,4,5},
                {6,7,8,9,10},
                {11,12,13,14,15},
                {16,17,18,19,20}

        };

        int[][] b = new int[][]{
                {1,2,3},
                {4,5,6}
        };

        int[][] c = new int[][]{
                {1,2,3,4},
                {5,6,7,8},
                {9,10,11,12}
        };
        System.out.println(spiralOrder(a));
        System.out.println(spiralOrder(a).size());
        System.out.println(spiralOrder(b));
        System.out.println(spiralOrder(b).size());
        System.out.println(spiralOrder(c));
        System.out.println(spiralOrder(c).size());

    }
}
