//
//  iExasAppDelegate.m
//  iExas
//
//  Created by Alex Gerdes on 14-04-10.
//  Copyright Gefion 2010. All rights reserved.
//

#import "iExasAppDelegate.h"
#import "iExasViewController.h"

@implementation iExasAppDelegate

@synthesize window;
@synthesize viewController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {    
    
    // Override point for customization after app launch    
    [window addSubview:viewController.view];
    [window makeKeyAndVisible];
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
