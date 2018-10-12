'use strict';

import React, { Component } from 'react';
import {
  StackNavigator,
} from 'react-navigation';

import {
  Platform,
  StyleSheet,
  Text,
  View
} from 'react-native';
import HomePage from './home';
import Maps from './maps';

const instructions = Platform.select({
  ios: 'Press Cmd+R to reload,\n' +
    'Cmd+D or shake for dev menu',
  android: 'Double tap R on your keyboard to reload,\n' +
    'Shake or press menu button for dev menu',
});

type Props = {};

const App = StackNavigator({
  Home: { screen: HomePage },
  Maps: { screen: Maps },
});

export default App;
