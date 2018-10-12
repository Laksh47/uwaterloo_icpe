package com.example.lakshmanan.matrixmultiplyandroid;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.text.method.ScrollingMovementMethod;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import java.util.Arrays;
import java.util.Random;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        final TextView textViewToChange = (TextView) findViewById(R.id.text1);

        final Button button = findViewById(R.id.button);

        button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                int matrix1 [][] = matrixGenerator();
                int matrix2 [][] = matrixGenerator();
                int matrix3 [][] = matrixMultiplication(matrix1, matrix2);
                String matrix3Str = Arrays.deepToString(matrix3);

                textViewToChange.setText(matrix3Str);
                textViewToChange.setMovementMethod(ScrollingMovementMethod.getInstance());
            }
        });
    }

    public static int[][] matrixGenerator(){
        int matrix [][] = new int[250][250];
        Random r = new Random( );
        for(int i=0; i < matrix.length; i++){
            for(int j=0; j < matrix[i].length; j++){
                matrix[i][j] = r.nextInt( 10000 );
            }
        }
        return matrix;
    }

    public static int[][] matrixMultiplication(int[][] matrix1, int[][] matrix2) {
        int m1rows = matrix1.length;
        int m1cols = matrix1[0].length;
        int m2cols = matrix2[0].length;

        int[][] result = new int[m1rows][m2cols];
        for (int i=0; i< m1rows; i++){
            for (int j=0; j< m2cols; j++){
                for (int k=0; k< m1cols; k++){
                    result[i][j] += matrix1[i][k] * matrix2[k][j];
                }
            }
        }
        return result;
    }
}