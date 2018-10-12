'use strict';

import React, { Component } from 'react';
import {
  StyleSheet,
  View,
  TextInput,
  Button,
} from 'react-native';

export default class HomePage extends Component<{}> {
  static navigationOptions = {
    title: 'Maps example',
  };

  constructor(props) {
    super(props);
    this.state = {};
  }

  _navigate = () => {
    this.props.navigation.navigate('Maps');
  };

  render() {
    return (
      <View style={styles.container}>
        <View style={styles.buttonStyle}>
          <TextInput
            underlineColorAndroid={'transparent'}
            style={styles.searchInput}
            onSubmitEditing={this._navigate}
            autoFocus = {true}
            placeholder='Search via name or postcode'/>
          <Button
            onPress={this._navigate}
            color='#48BBEC'
            title='Go'            
          />
        </View>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    // padding: 30,
    // marginTop: 65,
    // alignItems: 'center'
  },
  searchInput: {
    display: 'none'
  },
  buttonStyle: {
    width: 100,
  }
});