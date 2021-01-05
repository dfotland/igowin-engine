//
//  CustomView.m
//  MaskTest
//
//  Created by Sean Christmann on 12/22/08.
//  Copyright 2008 EffectiveUI. All rights reserved.
//

#import "MagnifierView.h"
#import "Board.h"
#import "BoardViewController.h"
#import "Game.h"

@implementation MagnifierView

@synthesize viewref, touchPoint, ghostMove;

- (id) initWithBoardController: (BoardViewController *) c {
	controller = c;
	board = controller.board;
	[self initWithFrame: controller.board.bounds];
	
	return self;
}

- (id)initWithFrame:(CGRect)frame {
    if (self = [super initWithFrame:frame]) {
        // Initialization code
		self.backgroundColor = [UIColor clearColor];
		ghostMove = -1;
		background = controller.woodgrain.image;
		
		[self start];
		
		unsigned int *data = malloc(4 * size * size);
		CGColorSpaceRef rgb = CGColorSpaceCreateDeviceRGB();
		if (!rgb) {
			NSLog(@"Can't create color space");
		}
		CGContextRef square = CGBitmapContextCreate(data, size, size, 8, size * 4, rgb, kCGImageAlphaPremultipliedLast);
		if (!square) {
			NSLog(@"Can't create bitmap");
		} else {
			int i, j;
			for (i = 0; i < size; ++i) {
				for (j = 0; j < size; ++j) {
					data[i * size + j] = 0xff << 24;
				}
			}
		}
		black = CGBitmapContextCreateImage(square);
    }
    return self;
}

- (void) start {
	size = 110;
	leftOffset = (size / 2) * -1;
	topOffset = 43;
	if (controller.game.boardSize == 9) {
		magnification = 7 / 10.;
	} else if (controller.game.boardSize == 13) {
		magnification = 6 / 8.;
	} else {
		magnification = 7 / 8.;
	}
}

- (void)drawRect:(CGRect)rect {
	if (touchPoint.y > board.bounds.size.height || touchPoint.y < 0) {
		return;
	}
	
	[self start];
	
	if (touchPoint.y < topOffset + size) {
		if (touchPoint.x < (board.bounds.size.width / 2)) {
			leftOffset -= (touchPoint.y - (topOffset + size));
			topOffset += (touchPoint.y - (topOffset + size));
			if ((touchPoint.x + leftOffset + size) > board.bounds.size.width) {
				leftOffset = board.bounds.size.width - size - touchPoint.x;
			}
		} else {
			leftOffset += (touchPoint.y - (topOffset + size));
			topOffset += (touchPoint.y - (topOffset + size));
			if ((touchPoint.x + leftOffset) < 0) {
				leftOffset = -touchPoint.x;
			}
		}
	}
	
	if (touchPoint.x < (size / 2)) {
		leftOffset -= (touchPoint.x - (size / 2));
	}
	
	if (touchPoint.x > (board.bounds.size.width - (size / 2))) {
		leftOffset -= (touchPoint.x - (board.bounds.size.width - (size / 2)));
	}
	
	if(cachedImage == nil) {
        UIGraphicsBeginImageContextWithOptions(self.bounds.size, NO, 0);
		//UIGraphicsBeginImageContext(self.bounds.size);
		[self.viewref.layer renderInContext:UIGraphicsGetCurrentContext()];
		cachedImage = [UIGraphicsGetImageFromCurrentImageContext() retain];
		UIGraphicsEndImageContext();
	}
	
	CGImageRef imageRef = [cachedImage CGImage];
	CGImageRef backgroundRef = [background CGImage];
	CGImageRef fullMask = [[UIImage imageNamed: @"loopmask.png"] CGImage];
	CGImageRef overlay = [[UIImage imageNamed: @"loop.png"] CGImage];
	
    float shownX = touchPoint.x * 2 - (size / magnification);
	float shownY = touchPoint.y * 2 - (size / magnification);
	float shownWidth = (size / magnification) * 2;
	float shownHeight = (size / magnification) * 2;
    float maskX = 0;
	float maskY = 0;
	float maskWidth = CGImageGetWidth(fullMask);
	float maskHeight = CGImageGetHeight(fullMask);
    float tmp;
    
    if (shownX < 0) {
		tmp = shownWidth;
		shownWidth = (shownWidth + shownX); //shownX will be negative here - we're subtracting what's off the board.
		shownX = 0; //Start showing from the edge
		maskX = ((tmp - shownWidth) / tmp) * maskWidth;
		maskWidth = maskWidth - maskX;
	} else if (shownX + shownWidth > board.bounds.size.width * 2) {
		tmp = shownWidth;
        shownWidth = board.bounds.size.width * 2 - shownX;
        maskWidth = (shownWidth / tmp) * maskWidth;
	}
	if (shownY < 0) {
		tmp = shownHeight;
		shownHeight = (shownHeight + shownY);
		shownY = 0;
		maskY = ((tmp - shownHeight) / tmp) * maskHeight;
		maskHeight = maskHeight - maskY;
	} else if (shownY + shownHeight > board.bounds.size.height * 2) {
		tmp = shownHeight;
		shownHeight = board.bounds.size.height * 2 - shownY;
		maskHeight = (shownHeight / tmp) * maskHeight;
	}
    
	//Create Mask
	CGRect shownArea = CGRectMake(shownX, shownY, shownWidth, shownHeight);
	
	CGRect maskArea = CGRectMake(maskX, maskY, maskWidth, maskHeight);
	
	CGImageRef maskRef = CGImageCreateWithImageInRect(fullMask, maskArea);
	CGImageRef mask = CGImageMaskCreate(CGImageGetWidth(maskRef), 
										CGImageGetHeight(maskRef),
										CGImageGetBitsPerComponent(maskRef), 
										CGImageGetBitsPerPixel(maskRef),
                                        CGImageGetBytesPerRow(maskRef), 
										CGImageGetDataProvider(maskRef), 
										NULL, 
										true);
	
	CGImageRef blackMask = CGImageMaskCreate(CGImageGetWidth(fullMask), 
										CGImageGetHeight(fullMask),
										CGImageGetBitsPerComponent(fullMask), 
										CGImageGetBitsPerPixel(fullMask),
                                        CGImageGetBytesPerRow(fullMask), 
										CGImageGetDataProvider(fullMask), 
										NULL, 
										true);
	
	CGImageRef xMaskedBlack = CGImageCreateWithMask(black, blackMask);
	
	CGImageRef subImage = CGImageCreateWithImageInRect(imageRef, shownArea);
	CGImageRef xMaskedImage = CGImageCreateWithMask(subImage, mask);
	
	CGImageRef subBackground = CGImageCreateWithImageInRect(backgroundRef, shownArea);
	CGImageRef xMaskedBackground = CGImageCreateWithMask(subBackground, mask);
	
	// Draw the image
	// Retrieve the graphics context
	CGContextRef context = UIGraphicsGetCurrentContext();
	CGAffineTransform xform = CGAffineTransformMake(
													1.0,  0.0,
													0.0, -1.0,
													0.0,  0.0);
	CGContextConcatCTM(context, xform);
    
    float areaX = touchPoint.x + leftOffset - 2;
    float areaY = -touchPoint.y + topOffset - 2;
    float areaHeight = size + 5;
    float areaWidth = size + 5;
    
    float area2X = touchPoint.x + leftOffset + (maskX * size / CGImageGetWidth(fullMask));
    float area2Y = -touchPoint.y + topOffset + ((CGImageGetHeight(fullMask) - maskY - maskHeight) * size / CGImageGetHeight(fullMask));
    float area2Width = maskWidth * size / CGImageGetWidth(fullMask);
    float area2Height = maskHeight * size / CGImageGetHeight(fullMask);
    
    //NSLog(@"X, Y: (%f, %f)", area2X, area2Y);
    
	CGRect area = CGRectMake(areaX, areaY, areaHeight, areaWidth);
	//CGRect area2 = CGRectMake(touchPoint.x + leftOffset + maskX, -touchPoint.y + topOffset + (CGImageGetHeight(fullMask) - maskHeight), size - maskX - (CGImageGetWidth(fullMask) - maskWidth), size - maskY - (CGImageGetHeight(fullMask) - maskHeight));
    CGRect area2 = CGRectMake(area2X, area2Y,
							  area2Width, area2Height);
	CGRect area3 = CGRectMake(touchPoint.x + leftOffset - 1, -touchPoint.y + topOffset - 1, size + 3, size + 3);
	
	CGContextDrawImage(context, area3, xMaskedBlack);
	CGContextDrawImage(context, area2, xMaskedBackground);
	CGContextDrawImage(context, area2, xMaskedImage);
	CGContextDrawImage(context, area, overlay);

	CGContextConcatCTM(context, xform);
	
	
	//if (ghostMove >= 0) {
		
		CGRect stoneArea = CGRectMake(touchPoint.x - ((board.spaceSize / 2) * magnification) + (leftOffset + size / 2), 
									  touchPoint.y - (topOffset + size / 2) - ((board.spaceSize / 2) * magnification), 
									  board.spaceSize * (magnification) - 1, board.spaceSize * (magnification) - 1);
		
		if ([controller.game getPlayerToMove] == BLACKCOLOR) {
			CGContextDrawImage(context, stoneArea, board.blackStone);
		} else {
			CGContextDrawImage(context, stoneArea, board.whiteStone);
		}
	//}
	
	CGImageRelease(maskRef);
	CGImageRelease(subImage);
	CGImageRelease(subBackground);
	CGImageRelease(blackMask);
	CGImageRelease(mask);
	CGImageRelease(xMaskedImage);
	CGImageRelease(xMaskedBlack);
	CGImageRelease(xMaskedBackground);
}


- (void)dealloc {
	[cachedImage release];
	[viewref release];
    [super dealloc];
}


@end
