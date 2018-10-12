import { Component, ViewChild } from '@angular/core';
import { IonicPage } from 'ionic-angular';
import { NavController } from 'ionic-angular';
// import { MapsPage } from '../maps/maps';

@IonicPage()
@Component({
  selector: 'page-home',
  templateUrl: 'home.html'
})
export class HomePage {

  constructor(public navCtrl: NavController) {

  }

  ionViewDidLoad(){
    console.log('HomePage loaded');
  }

  alert(){
  	console.log('clicked');
  	this.navCtrl.push('MapsPage');
  }
}