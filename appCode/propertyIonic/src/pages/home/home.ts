import { Component } from '@angular/core';
import { NavController } from 'ionic-angular';
import { HTTP } from '@ionic-native/http';

import { Directive, Renderer, ElementRef} from '@angular/core';

@Component({
  selector: 'page-home',
  templateUrl: 'home.html'
})

@Directive({ 
    selector: '[focuser]' // Attribute selector
})
export class HomePage {

  constructor(public navCtrl: NavController, private http: HTTP, private renderer:Renderer,
                private elementRef:ElementRef) {

  }

  ionViewDidEnter() {
    console.log('Did enter home.');
    setTimeout(() => {
        const element = this.elementRef.nativeElement.querySelector('input');
        this.renderer.invokeElementMethod(element, 'focus', []);
    }, 500);
  }

  // ngAfterViewInit() { 
  //   console.log("I am ng init view");
  //     // we need to delay our call in order to work with ionic ...
  //   // setTimeout(() => {
  //   //   const element = this.elementRef.nativeElement.querySelector('input');
  //   //   this.renderer.invokeElementMethod(element, 'focus', []);
  //   // }, 500);      
  // } 

  formSubmit(searchtext){
  	console.log("Hey:"+searchtext);
  	let str = this.urlForQueryAndPage('place_name', searchtext, 1);
    console.log(str);
    this.fetch(str);
  }

  handleLogin(searchtext) {
    this.formSubmit(searchtext);
  }

  fetch(str){
  	this.http.get(str, {}, {})
	  	.then(data => {

	    // console.log(data.status);
	    // console.log(data.data); // data received by server
	    // console.log(data.headers);
	    this.handleResponse(data.data);
	  })
	  .catch(error => {

	    console.log(error.status);
	    console.log(error.error); // error message as string
	    console.log(error.headers);

	  });
  }

  handleResponse(data){
  	// console.log(data);
  	// console.log("ghvbjkuy")
  	let listings = JSON.parse(data).response.listings;
  	// console.log(listings);
  	this.navCtrl.push('SearchPage', {listings: listings});
  }

  urlForQueryAndPage(key, value, pageNumber) {
	  const data = {
	      country: 'uk',
	      pretty: '1',
	      encoding: 'json',
	      listing_type: 'buy',
	      action: 'search_listings',
	      page: pageNumber,
	  };
	  data[key] = value;

	  const querystring = Object.keys(data)
	    .map(key => key + '=' + encodeURIComponent(data[key]))
	    .join('&');

	  return 'https://api.nestoria.co.uk/api?' + querystring;
	}
}
