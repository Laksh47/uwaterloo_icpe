/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 * @flow
 */

import React, { Component } from 'react';
import {
  Text,
  StyleSheet
} from 'react-native';
import MapView from 'react-native-maps';
import image from './images/flag-pink.png';

export default class Maps extends Component<{}> {
  render() {
    return (
      <MapView
        region={getDelta(38.4167, 112.7342, 20000)}
        style={StyleSheet.absoluteFillObject}>

      </MapView>
    );
  }
}

function getDelta(lat, lon, distance): Coordinates {
   const oneDegreeOfLatitudeInMeters = 111.32 * 1600;

   const latitudeDelta =distance / oneDegreeOfLatitudeInMeters;
   const longitudeDelta = distance / (oneDegreeOfLatitudeInMeters * Math.cos(lat * (Math.PI / 180)));

   return result = {
       latitude: lat,
       longitude: lon,
       latitudeDelta,
       longitudeDelta,
   }
}

function renderMarker({ location }) {
  return (
    <MapView.Marker
      image={image}
      coordinate={location}
    >
      <MapView.Callout>
        <Text>BiG BiG Callout</Text>
      </MapView.Callout>
    </MapView.Marker>
  );
}
