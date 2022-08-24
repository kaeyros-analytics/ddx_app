// $(document).ready(function(){
//     var $window = $(window);
//       $('section[data-type="background"]').each(function(){
//           var $bgobj = $(this); // assigning the object
      
//           $(window).scroll(function() {
//               var yPos = -($window.scrollTop() / $bgobj.data('speed')); 
              
//               // Put together our final background position
//               var coords = '50% '+ yPos + 'px';
  
//               // Move the background
//               $bgobj.css({ backgroundPosition: coords });
//           }); 
//       });    
//   });
  
  //menu transition js
  $(document).ready(function(){
    $('.x-logo-white').hide();
    $(".nav-link").css("font-weight", "bold");
    $(".nav-link").css("color", "white");
    $(window).scroll(function(){
        var scroll = $(window).scrollTop();
        console.log(scroll);
        if (scroll > 0) {
          $(".navbar").addClass("navbar-scroll");
          $(".navbar").removeClass("bg-transparent");
          $(".x-logo-init").show();
          $('.x-logo-white').hide();
        }
        else {
            $(".navbar").removeClass("navbar-scroll");
            $(".navbar").addClass("bg-transparent")
            $(".x-logo-init").show();
            $('.x-logo-white').hide();
        }
        if (scroll > 10) {
          $(".navbar").css("background-color", "#b7e3de");
          $(".navbar-brand").addClass("text-dark");
          $(".nav-link").addClass("text-dark");
          $(".nav-link").css("font-weight", "bold");
          $(".x-logo-init").hide();
          $('.x-logo-white').show();
        } else {
            $(".navbar").css("background-color", "transparent");
            $(".navbar-brand").removeClass("text-dark");
            $(".nav-link").removeClass("text-dark"); 
            $(".x-logo-init").show();
            $('.x-logo-white').hide(); 	
        }
    })
  })
  
