import { Component, ViewChild, ElementRef } from '@angular/core';
import { IonicPage, NavController } from 'ionic-angular';

/**
 * Generated class for the MapsPage page.
 *
 * See https://ionicframework.com/docs/components/#navigation for more info on
 * Ionic pages and navigation.
 */

declare var google;

@IonicPage()
@Component({
  selector: 'page-maps',
  templateUrl: 'maps.html',
})
export class MapsPage {

  @ViewChild('map') mapElement: ElementRef;
  map: any;

  constructor(public navCtrl: NavController) {

	}

	ionViewDidLoad(){
	  this.initMap();
	}

	initMap() {
	  this.map = new google.maps.Map(this.mapElement.nativeElement, {
	    zoom: 12,
	    center: {lat: 38.4167, lng: 112.7342},
	    streetViewControl: false,
	    mapTypeControl: false,
	    fullscreenControl: false
	  });
	}

}
