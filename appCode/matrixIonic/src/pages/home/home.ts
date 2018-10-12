import { Component } from '@angular/core';
import { NavController } from 'ionic-angular';

@Component({
  selector: 'page-home',
  templateUrl: 'home.html'
})
export class HomePage {
	value:string;

  constructor(public navCtrl: NavController) {
  	 // this.value="some value you want to fill"
  }

  alert(){
  	console.log('clicked');
  	this.multiply();
  }

  multiply(){
    console.log('heyyy')
    var matrix1 = this.matrixGenerator();
    var matrix2 = this.matrixGenerator();
    var matrix3 = this.matrixMultiplication(matrix1, matrix2);
    // console.log("Multiplied:");
    // console.log(matrix3.toString());
    this.value = matrix3.toString();
  };

  matrixGenerator(){
	  var matrix = new Array(250);
	  for (var i = 0; i < matrix.length; i++) {
	    matrix[i] = new Array(250);
	  }

	  for(var i=0; i < matrix.length; i++){
	      for(var j=0; j < matrix[i].length; j++){
	          matrix[i][j] = Math.floor(Math.random()*10000)
	      }
	  }
	  // printMatrix(matrix);
	  return matrix;
	}

	printMatrix(matrix) {
	  console.log('Printing:');
	  for (var i = 0; i<matrix.length; i++) {
	    for (var j = 0; j<matrix[0].length; j++) {
	      console.log(matrix[i][j]);
	    }
	  }
	}

	matrixMultiplication(matrix1, matrix2) {
	  var m1rows = matrix1.length;
	  var m1cols = matrix1[0].length;
	  var m2cols = matrix2[0].length;

	  //var result = new int[m1rows][m2cols];
	  var result = new Array(m1rows)
	  for (var i = 0; i < m1rows; i++) {
	    result[i] = new Array(m2cols);
	  }

	  console.log('Multiplying');

	  for (var i=0; i< m1rows; i++){
	      for (var j=0; j< m2cols; j++){
	          result[i][j] = 0;
	          for (var k=0; k< m1cols; k++){
	              result[i][j] += matrix1[i][k] * matrix2[k][j];
	              // console.log(result[i][j]);
	          }
	      }
	  }

	  // console.log(result.length);
	  // console.log(result[0].length);
	  return result;
	}
}
