// duration of scroll animation
var scrollDuration = 300;
// paddles
var leftPaddle = document.getElementsByClassName('left-paddle');
var rightPaddle = document.getElementsByClassName('right-paddle');
// get items dimensions
var itemsLength = $('.itemVariable').length;
var itemSize = $('.itemVariable').outerWidth(true);
console.log("=================== itemSize =============");
console.log(itemSize);
// get some relevant size for the paddle triggering point
var paddleMargin = 20;

// get wrapper width
var getfWrapperSize = function() {
	return $('.menu-wrapper').outerWidth();
}
var menuWrapperSize = getMenuWrapperSize();
// the wrapper is responsive
$(window).on('resize', function() {
	menuWrapperSize = getMenuWrapperSize();
    
});
// size of the visible part of the menu is equal as the wrapper size 
var menuVisibleSize = menuWrapperSize;

// get total width of all menu items
var getMenuSize = function() {
	return itemsLength * itemSize + 270;
};
var menuSize = getMenuSize();
// get how much of menu is invisible
var menuInvisibleSize = menuSize - menuWrapperSize;

// get how much have we scrolled to the left
var getMenuPosition = function() {
	return $('.menuVariable').scrollLeft();
};

// finally, what happens when we are actually scrolling the menu
$('.menuVariable').on('scroll', function() {

	// get how much of menu is invisible
	menuInvisibleSize = menuSize - menuWrapperSize;
	// get how much have we scrolled so far
	var menuPosition = getMenuPosition();

	var menuEndOffset = menuInvisibleSize - paddleMargin;

	// show & hide the paddles 
	// depending on scroll position
	if (menuPosition <= paddleMargin) {
		$(leftPaddle).addClass('hidden');
		$(rightPaddle).removeClass('hidden');
	} else if (menuPosition + 250 < menuEndOffset) {
		// show both paddles in the middle
		$(leftPaddle).removeClass('hidden');
		$(rightPaddle).removeClass('hidden');
	} else if (menuPosition + 250 >= menuEndOffset) {
        console.log("=========================== close end")
		$(leftPaddle).removeClass('hidden');
		$(rightPaddle).addClass('hidden');
}

});

let scrollPosition = 0;

// scroll to left
$(rightPaddle).on('click', function() {
    scrollPosition += itemSize;
	$('.menuVariable').animate( { scrollLeft: scrollPosition}, scrollDuration);
});

// scroll to right
$(leftPaddle).on('click', function() {
	console.log('Test');
    scrollPosition -= itemSize;
	$('.menuVariable').animate( { scrollLeft: scrollPosition }, scrollDuration);
	
});