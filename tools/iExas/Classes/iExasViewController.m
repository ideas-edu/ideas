//
//  iExasViewController.m
//  iExas
//
//  Created by Alex Gerdes on 14-04-10.
//  Copyright Gefion 2010. All rights reserved.
//

#import "iExasViewController.h"

@implementation iExasViewController



/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil]) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
}
*/


// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];

	// Create a service url
	NSString * urlString = @"http://ideas.cs.uu.nl/cgi-bin/ideas.cgi?input=%7B%22method%22:%22generate%22%2C%22params%22:%5B%22logic.dnf%22%2C1%5D%2C%22id%22:421%2C%22source%22:%22iphone%22%7D";

	NSString * encodedUrlString = @"http://ideas.cs.uu.nl/cgi/ideas.cgi?input="
	                              @"{\"method\" : \"generate\", \"params\" : [\"logic.dnf\", 1], \"id\" : 421}");
	
	// Create NSURL string from formatted string
	NSURL *url = [NSURL URLWithString:encodedUrlString];
	
	// Setup and start async download
	NSURLRequest *request = [[NSURLRequest alloc] initWithURL: url];
	NSURLConnection *connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
	[connection release];
	[request release];
}

id urlEncodeValue (NSString * str)
{
	NSString *result = (NSString *) CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault, (CFStringRef)str, NULL, CFSTR(":?#[]@!$&’()*+,;=\""), kCFStringEncodingUTF8);
	return [result autorelease];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data 
{
	// Store incoming data into a string
	NSString *jsonString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
	
	// Create a dictionary from the JSON string
	NSDictionary *results = [jsonString JSONValue];

	NSArray * r = [results objectForKey:@"result"];
	
	NSString * exercise = [r objectAtIndex:2];

	label.text = @"Beta is \u2227"; //exercise;
}


/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


- (void)dealloc {
    [super dealloc];
}

@end
