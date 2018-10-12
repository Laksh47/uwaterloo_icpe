import { Component } from '@angular/core';
import { IonicPage, NavController, NavParams } from 'ionic-angular';

/**
 * Generated class for the SearchPage page.
 *
 * See https://ionicframework.com/docs/components/#navigation for more info on
 * Ionic pages and navigation.
 */

@IonicPage()
@Component({
  selector: 'page-search',
  templateUrl: 'search.html',
})

export class SearchPage {
	public listings: Array<Object>;

  constructor(public navCtrl: NavController, public navParams: NavParams) {
  	this.listings = this.navParams.get('listings');
  	console.log(this.navParams.get('listings'))
  }

  ionViewDidLoad() {
    console.log('hmm ionViewDidLoad SearchPage');
  }

}
