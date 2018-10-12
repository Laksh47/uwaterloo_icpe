/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 * @flow
 */

import React, { Component } from 'react';
import {
  Platform,
  StyleSheet,
  Text,
  View,
  ScrollView,
  Button
} from 'react-native';

import {
  StackNavigator,
} from 'react-navigation';

type Props = {};

class MatrixMultiply extends Component<Props> {
  static navigationOptions = {
    title: 'Maps example',
  };

  constructor() {
    super();
    this.state = {
      myText: 'Welcome to React Native!'
    };
  }

  _multiply = () => {
    console.log('heyyy')
    var matrix1 = matrixGenerator();
    var matrix2 = matrixGenerator();
    var matrix3 = matrixMultiplication(matrix1, matrix2);
    // console.log("Multiplied:");
    // console.log(matrix3.toString());
    this.setState({myText: matrix3.toString()});
  };

  render() {
    return (
      <View style={styles.container}>
        <View style={styles.buttonStyle}>
          <Button
            onPress={this._multiply}
            color='#48BBEC'
            title='Multiply'            
          />
        </View>
        <ScrollView>
          <Text style={styles.welcome}>
            {this.state.myText}
          </Text>
        </ScrollView>
      </View>
    );
  }
}

function matrixGenerator(){
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

function printMatrix(matrix) {
  console.log('Printing:');
  for (var i = 0; i<matrix.length; i++) {
    for (var j = 0; j<matrix[0].length; j++) {
      console.log(matrix[i][j]);
    }
  }
}

function matrixMultiplication(matrix1, matrix2) {
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

const styles = StyleSheet.create({
  container: {
    // flex: 1,
    // justifyContent: 'center',
    // alignItems: 'center',
    // backgroundColor: '#F5FCFF',
  },
  welcome: {
    fontSize: 20,
    textAlign: 'center',
    margin: 10,
  },
  buttonStyle: {
    width: 100,
  }
});


const App = StackNavigator({
  Home: { screen: MatrixMultiply },
});
export default App;
