/* Start Addon_Clearfields */
(function($){

jQuery.fn.attachToField = function (id, defaultValue){
    var field = $('#'+id);
    var input_text = field.attr('value');
    return jQuery(field).bind('focus',function(){
       if( input_text == defaultValue ) {
          field.attr('value','');
        }
      }).bind('blur',function(){
        if(field.attr('value')==''){  
          field.attr('value',input_text);
        }
        input_text = field.attr('value');
      });
};
jQuery.init_clearfields = function(){

// attach events to search form input
      jQuery('#f2773088d2693368').attachToField('f2773088d2693368','Suchbegriff');
      jQuery('#f2773092d2693368').attachToField('f2773092d2693368','Suchbegriff');
      jQuery('#f2773070d2693570').attachToField('f2773070d2693570','Ihre E-Mail-Adresse');
      jQuery('#f2773064d2693548').attachToField('f2773064d2693548','Ihre E-Mail-Adresse');
      jQuery('#f2799986d2696900').attachToField('f2799986d2696900','search item');
      jQuery('#f2799956d2696900').attachToField('f2799956d2696900','search item');
      jQuery('#f2799956d2696904').attachToField('f2799956d2696904','yyyy.MM.dd');
      jQuery('#f2799956d2696906').attachToField('f2799956d2696906','yyyy.MM.dd');
return;
};

})(jQuery);
/* Ende Addon_Clearfields */
/* Start Addon_Printlink */
(function($){
jQuery.init_printlink = function(id){
    if(!$(id).length) {
      return;
    }
    pattern = $('<li id="navFunctionsPrint"><a href="#" title="' + PRINT_TOOLTIP + '">' + PRINT_PAGE_TEXT + '</a></li>');
$(id).prepend(pattern).find('#navFunctionsPrint a').click(function(){
        window.print();
        return false;
    });
    if(!$(id).is('ul')) {
      $('#navFunctionsPrint').wrap('<ul />');
    }
   return;
};
})(jQuery);
/* Ende Addon_Printlink */
/* Start Addon_Autosuggest */
var lang = document.getElementsByTagName('html')[0].lang.toUpperCase();
var action;
if (lang === "EN") {
   action = "SiteGlobals/Forms/Suche/EN/SolrVorschlagssuche_Form.html?nn=2693048";
} else {
   action = "SiteGlobals/Forms/Suche/DE/SolrVorschlagssuche_Form.html?nn=2693048";
}

(function(jquery) {
  jQuery.autoFunc = function (){
   jquery( "input[name=templateQueryString]" ).autocomplete({
    source: function( request, response ) {
      jquery.ajax({
        url: action,
        dataType: "json",
        data: {
          userQuery: request.term
        },
        success: function( data ) {
          if (data !== null && data.suggestions.length > 0){
          response( jquery.map( data.suggestions, function( item ) {
            return {
              label: item.name,
              value: "\""+item.name+"\""
            }
           }));
          }
          else {           
            return;          
          }
        }
      });
    },
    minLength: 2,
    select: function( event, ui ) {
      console.log( ui.item ?
        "Selected: " + ui.item.value :
        "Nothing selected, input was " + this.value );
      $(this).val(ui.item.value);
    //  $(this).closest("form").submit();
    }
  });
 };
})($);
/* Ende Addon_Autosuggest */
jQuery(document).ready(function (){
   $ ('body').removeClass('js-off').addClass('js-on');
   $ .init_clearfields();
   $ .init_printlink('#navFunctions');
   $ .autoFunc();

});