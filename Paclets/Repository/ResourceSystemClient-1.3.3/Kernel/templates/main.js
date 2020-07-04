$(function() {
 
  // var $shortName = $('main p.wl-input .short-name')
  // $shortName.html($shortName.html().replace(':',':<wbr>'))
  
  var $details = $('main .details')
  if ($details.height() > 63) {
    $details.addClass('hide').children().first().after('<div class=ellipsis></div><button class="readmore">Show more »</button>').next().next().one('click', function() {
      $(this).remove();
      $details.removeClass('hide');
      $('#page-nav > nav').affix('checkPosition');
    });
  }
  
  $('#page-nav > nav').affix({
    offset: {
      top: function () { return $('.row-frame').offset().top - 42; }
    }
  }); 
  
  new Clipboard('.example-frame .input td img', {
    text: function (trigger) {
      return trigger.getAttribute('alt');
    }
  }).on('success', function (e) {
    var $input = $(e.trigger);
    $input.addClass('clipped');
    setTimeout(function (){ $input.removeClass('clipped') }, 5000);
  });
  
});