//
//  iExasAppDelegate.h
//  iExas
//
//  Created by Alex Gerdes on 14-04-10.
//  Copyright Gefion 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@class iExasViewController;

@interface iExasAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    iExasViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet iExasViewController *viewController;

@end

