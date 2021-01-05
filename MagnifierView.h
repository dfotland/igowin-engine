//
//  CustomView.h
//  MaskTest
//
//  Created by Sean Christmann on 12/22/08.
//  Copyright 2008 EffectiveUI. All rights reserved.
//

#import <UIKit/UIKit.h>

@class BoardViewController;
@class Board;

@interface MagnifierView : UIView {
	UIView *viewref;
	CGPoint touchPoint;
	UIImage *cachedImage;
	UIImage *background;
	CGImageRef black;
	BoardViewController *controller;
	Board *board;
	int ghostMove;
	int size;
	float leftOffset;
	float topOffset;
	float magnification;
}

@property(nonatomic, retain) UIView *viewref;
@property(assign) CGPoint touchPoint;
@property int ghostMove;

- (id) initWithBoardController: (BoardViewController *) c;

@end
