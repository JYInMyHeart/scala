import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @Author: xck
 * @File: Test12
 * @Time: 14:31 2019/5/27
 */
public class Test12 {
    public List<List<String>> solveNQueens(int n) {
        List<List<String>> res = new ArrayList<>();
        char[][] test = new char[n][n];
        for(int i = 0; i < n; i++)
            for(int j = 0; j < n; j++)
                test[i][j] = '.';
        helper(res,test,0,n);
        return res;
    }

    public void helper(List<List<String>> res,char[][] test,int row,int n){
        if(row == n){
            List<String> collect = Arrays.stream(test).map(String::new).collect(Collectors.toList());
                res.add(collect);
                return;
        }
        for (int i = 0; i < n; i++) {
            if(valid(test,i,row,n)){
                test[row][i] = 'Q';
                helper(res,test,row + 1,n);
                test[row][i] = '.';
            }
        }
    }

    public boolean valid(char[][] test,int col,int row,int n){
        for (int i = 0; i < row; i++) {
            if(test[i][col] == 'Q')
                return false;
        }

        for (int i = row - 1,j = col - 1; i >= 0 && j >= 0 ; i--,j--) {
            if(test[i][j] == 'Q')
                return false;
        }

        for (int i = row - 1,j = col + 1; i >= 0 && j < n ; i--,j++) {
            if(test[i][j] == 'Q')
                return false;
        }
        return true;
    }

    public static void main(String[] args) {
        Test12 test12 = new Test12();
        test12.solveNQueens(8).stream().forEach(x -> {
            x.stream().forEach(System.out::println);
            System.out.println("------------------------");
        });


    }
}
