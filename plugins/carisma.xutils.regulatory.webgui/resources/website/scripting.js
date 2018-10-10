var fenster;

//==================================================================================
// functions to be called at the start of the page 
// global functions
//==================================================================================

	// tries to prevent the browser from showing the context menu for the specific element
function changeOnContextMenu() {
	document.oncontextmenu = function(e) {
		return false; // this prevents the default context from beeing shown
	}
}

function $(id) {
	return document.getElementById(id);
}

	//determine the size of the window
function getWindowSize(){	
	$('main_div').style.width = window.innerWidth * 0.98 + "px";
	$('main_div').style.height = window.innerHeight * 1.8 + "px";
	var diff = window.innerWidth/window.innerHeight;	// determine if the resolution is in 4:3 or 16:9
	if (window.screen.width == 1024 && window.screen.height == 768) {
		$('textOfTheRule').style.height = "36%";
		$('selectField').size = "10";
		var checkboxes = document.getElementsByName('checkboxDiv');
		for (var i = 0; i < checkboxes.length; i++) {
			checkboxes[i].style.marginTop = "3%";
		}
		$('relInline').style.marginTop = "3%";
		$('submitButton').value = "RE erstellen";
		$('declineButton').value = "RE entfernen";
		$('titleSituations').style.marginRight = "5%";
		$('titleRefereces').style.marginRight = "5%";
		$('navigationList').style.overflowX = "visible";
		$('image').style.fontSize = "0.9em";
		$('upperContent').style.fontSize = "1.3em";
		$('lowerContent').style.fontSize = "1.3em";
		$('ruleelementTypes').style.fontSize = "1.3em";
		$('situationButton').style.width = "50%";
		$('titleConstraints').style.marginTop = "40%";
		$('constraintButton').style.marginTop = "40%";
	}
	else if (diff < 1.6) {
		$('textOfTheRule').style.height = "40%";
		$('selectField').size = "14";
		var checkboxes = document.getElementsByName('checkboxDiv');
		for (var i = 0; i < checkboxes.length; i++) {
			checkboxes[i].style.marginTop = "10%";
		}
	} else {
		$('textOfTheRule').style.height = "36%";
		$('selectField').size = "10";
		var checkboxes = document.getElementsByName('checkboxDiv');
		for (var i = 0; i < checkboxes.length; i++) {
			checkboxes[i].style.marginTop = "3%";
		}
		$('relInline').style.marginTop = "3%";
	}
}
function cancel() {
	window.close();
}

/*
 * Fuegt den Listeneintraegen Eventhandler und CSS Klassen hinzu,
 * um die Menuepunkte am Anfang zu schliessen.
 *
 * menu: Referenz auf die Liste.
 * data: String, der die Nummern aufgeklappter Menuepunkte enthaelt.
 */
function treeMenu_init(menu, data, parentWindow) {
	if (parentWindow == true) {
		window.opener.$('textOfTheRule').value = "";
	} else {
		$('textOfTheRule').value = "";
	}
	var elementArray = document.getElementsByName("RuleElement");
	for(var i = 0; i < elementArray.length; i++){
		elementArray[i].checked = false;
	}
  var array = new Array(0);
  if(data != null && data != "") {
    array = data.match(/\d+/g);
  }
  var items = menu.getElementsByTagName("li");
  for(var i = 0; i < items.length; i++) {
		      items[i].onclick = treeMenu_handleClick;
		      if(!treeMenu_contains(treeMenu_getClasses(items[i]), "treeMenu_opened")
		          && items[i].getElementsByTagName("ul").length
		            + items[i].getElementsByTagName("ol").length > 0) {
		        var classes = treeMenu_getClasses(items[i]);
		        if(array.length > 0 && array[0] == i) {
		          classes = new Array("treeMenu_open");
		        }
		        else {
		          classes = new Array("treeMenu_closed");
		        }
		        items[i].className = classes.join(" ");
		        if(array.length > 0 && array[0] == i) {
		          array.shift();
		        }
		      }
  }
 
}

/*
 * ?ndert die Klasse eines angeclickten Listenelements, sodass
 * ge?ffnete Men?punkte geschlossen und geschlossene ge?ffnet
 * werden.
 *
 * event: Das Event Objekt, dass der Browser ?bergibt.
 */
function treeMenu_handleClick(event) {
  if(event == null) { //Workaround f?r die fehlenden DOM Eigenschaften im IE
    event = window.event;
    event.currentTarget = event.srcElement;
    while(event.currentTarget.nodeName.toLowerCase() != "li") {
      event.currentTarget = event.currentTarget.parentNode;
    }
    event.cancelBubble = true;
  }
  else {
    event.stopPropagation();
  }
  // if(event.currentTarget.firstChild.nodeName.toLowerCase() == "input"){
    	// alert("input found!");
    // } else {
    	// alert(event.currentTarget.nodeName);
    // }
  var array = treeMenu_getClasses(event.currentTarget);
  for(var i = 0; i < array.length; i++) {	    	
	      if(array[i] == "treeMenu_closed") {
	        array[i] = "treeMenu_opened";
	      }
	      else if(array[i] == "treeMenu_opened") {
	        array[i] = "treeMenu_closed";
	      }
  }
  event.currentTarget.className = array.join(" ");
}

/*
 * Gibt alle Klassen zur?ck, die einem HTML-Element zugeordnet sind.
 *
 * element: Das HTML-Element
 * return: Die zugeordneten Klassen.
 */
function treeMenu_getClasses(element) {
  if(element.className) {
    return element.className.match(/[^ \t\n\r]+/g);
  }
  else {
    return new Array(0);
  }
}

/*
 * ?berpr?ft, ob ein Array ein bestimmtes Element enth?lt.
 *
 * array: Das Array
 * element: Das Element
 * return: true, wenn das Array das Element enth?lt.
 */
function treeMenu_contains(array, element) {
  for(var i = 0; i < array.length; i++) {
    if(array[i] == element) {
      return true;
    }
  }
  return false;
}

/*
 * Gibt einen String zur?ck, indem die Nummern aller ge?ffneten
 * Men?punkte stehen.
 *
 * menu: Referenz auf die Liste
 * return: Der String
 */
function treeMenu_store(menu) {
  var result = new Array();;
  var items = menu.getElementsByTagName("li");
  for(var i = 0; i < items.length; i++) {
    if(treeMenu_contains(treeMenu_getClasses(items[i]), "treeMenu_opened")) {
      result.push(i);
    }
  }
  return result.join(" ");
}


//=============================================================================================
// functions called from WebOntology.html
//=============================================================================================

	// loads the constraints
function loadConstraints() {
	removeAllChilds('constraints');
	 if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		xmlhttp.open("GET", "WebOntology.html?Constraint", true);
		xmlhttp.send();  
		console.log("Request send from loadConstraints: " + "Constraint");
		xmlhttp.onreadystatechange=function()
		  {
			  if (xmlhttp.readyState==4)
			    {
			    	var constraints = xmlhttp.responseText.split("?");
			    	var ul1 = document.createElement('UL');
			    	ul1.id = "menu4";
			    	for ( var j = 0; j < constraints.length; j++) {
			    		if (constraints[j] != "") {
			    			var li1 = document.createElement('LI');
			    			var spanName = document.createElement('SPAN');
			    			var ul2 = document.createElement('UL');
			    				// get the parts of the constraint
			    			var constraintMembers = constraints[j].split("&");
			    			spanName.innerHTML = constraintMembers[0].replace(new RegExp('_', 'g'), ' ');
			    			spanName.id = constraintMembers[0];
			    			spanName.className = "constraintName";
			    			for (var k = 0; k < constraintMembers.length; k++) {
			    				if (constraintMembers[k].indexOf("ruleElements") != -1){
			    					createConstraintRuleElements(constraintMembers[k], ul2);
				    			} else if (constraintMembers[k].indexOf("individuals") != -1) {
				    				if (constraintMembers[k].indexOf(",") != -1) {
				    					createConstraintIndividuals(constraintMembers[k], ul2);
					    			}
				    			}
				    			li1.appendChild(spanName);
							    li1.appendChild(ul2);
							    ul1.appendChild(li1);
			    			}
						}
			    	}
			    	if (li1 != null) {
				    	$('constraints').appendChild(ul1);
				    	treeMenu_init($('menu4'), '', false);
				    }
			    	
			    }
		 }
}


	// fetches the childs of the given rule
function getChilds(ruleName, idWhereToAppend, idOfTheParentElement){
	var xmlhttp;
	if (window.XMLHttpRequest)
	  {// code for IE7+, Firefox, Chrome, Opera, Safari
	  xmlhttp=new XMLHttpRequest();
	  }
	else
	  {// code for IE6, IE5
	  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
	xmlhttp.open("GET", "WebOntology.html?" + ruleName, true);
	xmlhttp.send();  
	xmlhttp.onreadystatechange=function()
	  {
		  if (xmlhttp.readyState==4)
		    {
		   		var text = xmlhttp.responseText;
		   		var textArray = text.split("?");
		   		var nameOfTheNode = $(idOfTheParentElement).textContent;
		   		nameOfTheNode = nameOfTheNode.split(" ")[0];
		   		textArray.sort();			   		
		   		for(var j = 0; j < textArray.length; j++) 
		   		{
		   			if(textArray[j] != ""){
			   			if(j < textArray.length - 1){
				   			var li = document.createElement("li");
				   			var div = document.createElement("div");
				   			var span = document.createElement("span");
				   			span.innerHTML = textArray[j];
				   			span.style.font = ".9em Times";
				   			span.setAttribute('onclick', 'getInformationAboutRule(\'' + nameOfTheNode + '=' + textArray[j] + '\')');
				   			div.appendChild(span);
				   			li.appendChild(div);
				   			li.className = "";
				   			li.id = "entry";
				   			$(idWhereToAppend).appendChild(li);
			   			} else {
			   				var li = document.createElement("li");
				   			var div = document.createElement("div");
				   			var span = document.createElement("span");
				   			span.innerHTML = textArray[j];
				   			span.style.font = ".9em Times";
				   			span.setAttribute('onclick', 'getInformationAboutRule(\'' + nameOfTheNode + '=' + textArray[j] + '\')');
				   			div.appendChild(span);
				   			li.appendChild(div);
				   			li.className = "ende";
				   			li.id = "entry";
				   			$(idWhereToAppend).appendChild(li);
			   			}
			   		}
		   		}
		   		treeMenu_init($('menu'), '', false);	
		    }
	  }
	  	// to load the part only once
	  $(idOfTheParentElement).onmouseover = null;	 
}

	// deletes a rule element
function deleteRuleElement() {
	removeAllChilds('image');
    var userSelection = window.getSelection();
    if( userSelection.anchorNode.parentNode.nodeName == userSelection.focusNode.parentNode.nodeName &&
	    userSelection.anchorNode.parentNode.nodeName == "SPAN" ) {
	    var prevNode = userSelection.anchorNode.parentNode.previousSibling;
		var offset = 0;
	    while( prevNode )
	    {
	        if( prevNode.nodeName == "SPAN" )
	            offset += prevNode.firstChild.length;
	        else    
	            offset += prevNode.length;
	            
	        prevNode = prevNode.previousSibling;
	    }
	    	
	    var range = document.createRange();	
	
	    range.setStart( userSelection.anchorNode, userSelection.anchorOffset );
	    range.setEnd( userSelection.focusNode, userSelection.focusOffset ); 
	    xmlhttp.open("GET", "WebOntology.html?delete&name=" + userSelection.anchorNode.parentNode.innerHTML 
	    			+ "&element=" + $('ruleName').innerHTML
	    			+ "&spos=" + (range.startOffset + offset) 
	    			+ "&epos=" + (range.endOffset + offset));
		xmlhttp.send();
		console.log("Request send from deleteRuleElement: " + "delete&name=" + userSelection.anchorNode.parentNode.innerHTML 
	    			+ "&element=" + $('ruleName').innerHTML
	    			+ "&spos=" + (range.startOffset + offset) 
	    			+ "&epos=" + (range.endOffset + offset));
			xmlhttp.onreadystatechange=function()
			  {
			   if (xmlhttp.readyState==4)
			    {
			    	if (xmlhttp.responseText == "error") {
			    		var image = new Image();
						image.src = 'bilder/red_x.gif';
						image.id = "img";
						image.value = "error";
						$("image").appendChild(image);
						var element = document.createElement("span");
						element.id = "errorElement";
						element.innerHTML = "" + userSelection.anchorNode.parentNode.innerHTML + " konnte nicht gespeichert werden!";
						$("image").appendChild(element);
			    	} else {
			    		storeOntology(userSelection.anchorNode.parentNode.innerHTML);
			    	}			    							   									   		
			    }
			  }
	} else {
		alert("Bitte ein hervorgehobenes Wort markieren.");
	}
}

	// prepares the creation of a new Rule Element
function prepareRuleElementCreation(){
	removeAllChilds("image");	
	var counter = 0;
	var name; var ruleElement; var array; var checkbox; var elType;
	if ($('textOfTheRule').name == undefined) {
		alert("Bitte zuerst eine Regel auswählen.");
		return;
	}
	var textArray = $('textOfTheRule').name.split("=");
	checkbox = document.getElementsByName("RuleElement");
	for(var i = 0; i < checkbox.length; i++)
	{

		if(checkbox[i].checked == true)
		{
			counter = counter + 1;
			ruleElement = checkbox[i].value;
			checkbox[i].checked = false;
			array = selectedText();
			if (array == undefined) {
				alert("Interner Fehler! Array nicht definiert");
				return;
			}
			if (array.length != 3) {
				alert("Interner Fehler! Array hat falsche Groesse");
				return;
			}
			text = array[2];
			if(text == "")
			{
			  	return;
			} 		
			if (text.toString().charAt(0) == " ") {
				array[0] = array[0] + 1;
			}
			if (text.toString().charAt(text.toString().length - 1) == " ") {
				array[1] = array[1] - 1;
			}
			
				// replace the first and last whitespace
			text = text.toString().replace(/^(\s|\u00A0)+|(\s|\u00A0)+$/g, ''); 
			
				
			name = textArray[1];
				// create the url to send the changes to the server
			doc = "WebOntology.html?storeRuleElement=" + name + "&className=" + ruleElement + "&elName=" + text + 
			"&spos=" + array[0] + "&epos=" + array[1] + "&elType=";
				// open the popup window
			openWindow('CreateNewRuleElement.html', doc, 600, 400);
			return;
		}
	 }
	if (counter == 0) {
		alert("Bitte eine Checkbox auswählen! ");
	}

	}

	// prepares the creation of a new situation
function prepareNewSituation() {
	removeAllChilds('image');
	
	var rule = $('ruleName').innerHTML;
	if (rule == " ") {
		alert('Bitte zuerst eine Regel auswählen.');
		return;
	} else {
		var request = "storeSituation=" + rule + "?" + "ruleelements=";
		var options = document.ruleElementForm.selection.options;
		for (var i = 0; i < options.length; i++) {
			if (options[i].selected == true) {
				request = request + "&" + options[i].value;
			}
		}
		if (request == "storeSituation=" + rule + "?" + "ruleelements=") {
			alert("Bitte Regelelemente auswählen.");
			return;
		}
		openWindow('CreateNewSituation.html', request, 900, 400);
	}
	
}
	// prepares the new constraint window
function prepareNewConstraint() {
	fenster = window.open('CreateNewConstraintClass.html', "newConstraint", "width=900,height=400,status=yes,scrollbars=yes,resizable=no");
   	fenster.focus();
}

	// deselect all rule elements in the rule element list
function deselectAllRuleElements() {
	var selectionChilds = document.getElementsByTagName('OPTION');
		// deselect all elements in the selection form
	for (var i = 0; i < selectionChilds.length; i++) { 
		selectionChilds[i].selected = false; 
	}
}
	// select all rule element which belong to the focussed situation
function selectRelatedRuleElements(node) {
	var situationChilds = document.getElementsByClassName(node);
	var selectionChilds = document.getElementsByTagName('OPTION');
	deselectAllRuleElements();
		// select all matching elements in the selection form
	for (var i = 0; i < situationChilds.length; i++) {
		for (var j = 0; j < selectionChilds.length; j++) {			
			if (situationChilds[i].id == selectionChilds[j].value) {
				selectionChilds[j].selected = true;
			}
		}
	}
}
	// prepares the window for the ruleElement relation
function prepareRuleElementsRelation() {
	if (document.getElementsByTagName('OPTION').length < 1) {
		alert("Es sind keine Regelelemente vorhanden.");
		return;
	}
	fenster = window.open('CreateRelatedRuleElements.html', "relatedRuleElements", "width=900,height=400,status=yes,scrollbars=yes,resizable=no");
   	fenster.focus();
}

function loadRelatedRuleElements() {
	var options = $('OPTION');
	if (window.XMLHttpRequest)
	  {// code for IE7+, Firefox, Chrome, Opera, Safari
	  xmlhttp=new XMLHttpRequest();
	  }
	else
	  {// code for IE6, IE5
	  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
	var request = "";
	for (var i = 0; i < options.length; i++) {
		request = request + options[i].value + ",";
		
	}
	xmlhttp.open("GET", "WebOntology.html?relatedRuleElements=" + request, true);
			xmlhttp.send();  
			xmlhttp.onreadystatechange=function()
			{
				  if (xmlhttp.readyState==4)
				    {
				    	var response = xmlhttp.responseText.split(",");
						for (var i = 0; i < response.length - 1; i++) {
							var tooltip = "Regelelement steht in Relation zu ";
							var ruleElements = response[i].split("=");
							var idOfTheRuleelement = ruleElements[0] + "_id";
							var relatedRuleElements = ruleElements[1].split("&");
							for (var j = 0; j < relatedRuleElements.length - 1; j++) {
								tooltip = tooltip + relatedRuleElements[j];
								if (j < relatedRuleElements.length - 2) {
									tooltip = tooltip + ", ";
								}
							}
							$(idOfTheRuleelement).title = tooltip;
						}
				    	
				    }
			}
}


//============================================================================================
// functions called from CreateNewRuleElement.html
//============================================================================================

	//retrieves the content for the rule element creation
function retrieveContent() {
	var doc = window.name;
	var request = doc.substring(doc.indexOf('className=') + 10, doc.indexOf('&elName'));
	$('insert').innerHTML = doc.substring(doc.indexOf('elName=') + 7, doc.indexOf('&spos'));
	$('createNewRuleElementType').value = doc.substring(doc.indexOf('elName=') + 7, doc.indexOf('&spos'));
	$('createNewRuleElementType').select();
   	if (window.XMLHttpRequest)
		  {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
		  }
		else
		  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
		  }
		  xmlhttp.open("GET", "WebOntology.html?" + request, true);
			xmlhttp.send();  
			console.log("Request send from retrieveContent: " + request);
			xmlhttp.onreadystatechange=function()
			  {
				  if (xmlhttp.readyState==4)
				    {
				    	var response = xmlhttp.responseText.split("?");
				    	for ( var i = 0; i < response.length; i++) {
				    		var newNode = document.createElement('span');
				    		var br = document.createElement('br');
				    		newNode.id = 'subElements';
				    		newNode.name = response[i];
				    		newNode.setAttribute('ondblclick', 'document.getElementById(\'createNewRuleElementType\').value = this.innerHTML');
				    		newNode.innerHTML = response[i].substring(response[i].indexOf("_") + 1);
				    		$('createNewRuleElementChoice').appendChild(newNode);
				    		$('createNewRuleElementChoice').appendChild(br);
				    	}
				    }
			  }
   
}	

	// creates a new rule element
function createNewRuleElement() {	
	var xmlhttp;	// the Request Object
	var elementType = $('createNewRuleElementType').value;	// the element were the element name is written
	if (elementType == "" || elementType == 'undefined') {	// if something goes wrong
		alert("Bitte einen Namen eingeben!");
		return;
	} 
	var doc = window.name;
		if (window.XMLHttpRequest)
		  {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
		  }
		else
		  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
		  }
	
	xmlhttp.open("GET", doc + elementType, true);
	xmlhttp.send();  
	console.log("Request send from createNewRuleElement: " + doc + elementType);
	xmlhttp.onreadystatechange = function () {
		if (xmlhttp.readyState==4) {
			if (xmlhttp.responseText == "succesful") {
				var checkbox = window.opener.document.getElementsByName("RuleElement");
				
				window.opener.storeOntology(doc.substring(doc.indexOf('elName=') + 7, doc.indexOf('&spos')));
				
			} else {
				var image = new Image();
				image.src = 'bilder/red_x.gif';
				image.id = "img";
				image.value = "error";
				window.top.$("image").appendChild(image);
				var element = document.createElement("span");
				element.id = "errorElement";
				element.innerHTML = "" + doc.substring(doc.indexOf('elName=') + 7, doc.indexOf('&spos')) + " konnte nicht gespeichert werden!";
				window.top.$("image").appendChild(element);
			}
			
			
			window.close();
		} 
	}
	
	
}
 
//============================================================================================
// functions used to move the elements within the pop ups
//============================================================================================

	// adds the selected element to the choosen list
function add() {
	var options = document.rightForm.selection.options;
	for (var i = 0; i < options.length; i++) {
		if (options[i].selected == true) {
			addRE(options[i].innerHTML);
		}
	}
}
	// removes the selected element from the choosen list
function remove() {
	var nodes = document.leftForm.selected.options;
	var selection;
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].selected == true) {
			selection = nodes[i].id;
		}
	}
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].id == selection) {
				document.leftForm.selected.removeChild(nodes[i]);
				return;
		}
	}
	alert("Kein Element in dem linken Feld ausgewählt.");
}
	// moves the selected element uppwards on the choosen list
function moveUp() {
	var nodes = document.leftForm.selected.options;
	var selection;
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].selected == true) {
			selection = nodes[i].id;
		}
	}
	for (var i = 1; i < nodes.length; i++) {
		if (nodes[i].id == selection) {
				var buffer = nodes[i - 1].innerHTML;
				var bufferId = nodes[i -1].id;
				nodes[i - 1].innerHTML = nodes[i].innerHTML;
				nodes[i - 1].id = nodes[i].id;
				nodes[i].innerHTML = buffer;
				nodes[i].id = bufferId;
				nodes[i].selected = false;
				nodes[i - 1].selected = true;
				return;
		}
	}
}
	// moves the selected element downwards on the choosen list
function moveDown() {
	var nodes = document.leftForm.selected.options;
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].selected == true) {
			selection = nodes[i].id;
		}
	}
	for (var i = 0; i < nodes.length - 1; i++) {
		if (nodes[i].id == selection) {
				var buffer = nodes[i + 1].innerHTML;
				var bufferId = nodes[i + 1].id;
				if (buffer != " ") {
					nodes[i + 1].innerHTML = nodes[i].innerHTML;
					nodes[i + 1].id = nodes[i].id;
					nodes[i].innerHTML = buffer;
					nodes[i].id = bufferId;
					nodes[i].selected = false;
					nodes[i + 1].selected = true; 
				}
				return;
				
		}
	}
}

//============================================================================================
// functions called from CreateNewSituation.html
//============================================================================================

	// loads the existing constraint classes
function loadExistingConstraints() {
	var constraintClasses = window.opener.document.getElementsByClassName('constraintName');
	for (var i = 0; i < constraintClasses.length; i++) {
		var option = document.createElement("OPTION");
		option.value = constraintClasses[i].id;
		option.innerHTML = constraintClasses[i].innerHTML;
		$('selectionForm').appendChild(option);
	
	}
}

function selectConstraints() { 	
	var request = window.name;
	var name = $('newName').value;
	var content = request.split("?");
	var ruleelements;
	for (var i = 0; i < content.length; i++) {
		if (content[i].indexOf("ruleelements") != -1) {
			ruleelements = content[i].substring(content[i].indexOf("=") + 1, content[i].length).split("&");
			if (content[i].length == content[i].indexOf("=") + 1) {
				ruleelements = undefined;
			}
		} 		
	}
	if (ruleelements == undefined) {
		alert("Keine Regelelemente ausgewählt.");
		window.close();
		return;
	}
	if (name == "") {
		alert("Bitte einen Namen eingeben.");
		return;
	} else {
		var newRequest = request + "?" + "SituationName=" + name;
		var selectedConstraints = $('selectedForm').childNodes;
		var requestMembers = newRequest.split("?");
		window.name = requestMembers[0] + "?" + requestMembers[2];
		var constraints = "";
		for (var i = 0; i < selectedConstraints.length; i++) {
			if (selectedConstraints[i].innerHTML != undefined) {
				constraints = constraints + selectedConstraints[i].innerHTML + "§";				
			}
		}
		if (constraints == "") {
			alert("Bitte Constraints auswählen.");
			window.name = request;
			return;
		}
		openConstraintWindow(window.name + "%" + constraints, ruleelements);
		window.close();
	}
}

//=============================================================================================
// functions called from CreateNewConstraint.html
//=============================================================================================

	// loads the content of the constraint window
function loadConstraintWindow(){
	$('newName').value = "Name des Constraints";
	$('newName').select();
	removeAllChilds('selectedForm');
	removeAllChilds('selectionForm');
	var name = window.name;
	var windowNameContent = name.split("%");
	name = windowNameContent[1];
	var constraintList = name.split("&")[0];
	var constraint = constraintList.substring(0, constraintList.indexOf("§"));
	if (constraint == "") {
		var partsOfTheRequest = window.name.split("%");
		window.name = partsOfTheRequest[0] + partsOfTheRequest[1].
						substring(partsOfTheRequest[1].indexOf("?C"), partsOfTheRequest[1].length);
		sendNewSituation();
		window.close();
		return;
	}
	var constraints = name.split("&");
	window.name = windowNameContent[0] + "%" + name.substring(name.indexOf(constraint) + constraint.length + 1, name.length);
	var contentMembers = constraints[1].split(",");
	$('specification').innerHTML = constraint;
	for (var i = 1; i < contentMembers.length; i++) {
		if (contentMembers[i] != "") {
			if (contentMembers[i].indexOf("?") != -1) {
				contentMembers[i] = contentMembers[i].substring(0, contentMembers[i].indexOf("?"));
			}
			var option = document.createElement('OPTION');
			option.innerHTML = contentMembers[i].replace(new RegExp('_', 'g'), " ");
			option.value = contentMembers[i];
			$('selectionForm').appendChild(option);
		}
		
	}
	if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		xmlhttp.open("GET", "WebOntology.html?ConstraintSpecification=" + constraint.replace(new RegExp(' ', 'g'), "_"), true);
		xmlhttp.send();
		xmlhttp.onreadystatechange=function()
		{
			  if (xmlhttp.readyState==4)
			    {
			    	if (xmlhttp.responseText != "error") {
			    		var ruleElements = xmlhttp.responseText.substring(xmlhttp.responseText.indexOf("=") + 1).split(",");
			    		for (var i = 0; i < ruleElements.length; i++) {
			    			var option = document.createElement('OPTION');
			    			option.innerHTML = ruleElements[i].replace(/^(\s|\u00A0)+|(\s|\u00A0)+$/g, '');
			    			option.value = "";
			    			$('selectedForm').appendChild(option);
			    		}
			    	} else {
			    		alert("Keine Regelelemente gefunden.");
			    	}
			    }
			    
		}
	
}

	// adds a rule element to the choosen list
function addREForConstraint() {
	var options = document.rightForm.selection.options;
	var leftOptions = document.leftForm.selected.options;
	var actualRuleElement;
	for (var i = 0; i < options.length; i++) {
		if (options[i].selected == true) {
			actualRuleElement = options[i].innerHTML;
			for (var j = 0; j < leftOptions.length; j++) {
				if (leftOptions[j].value == "" && leftOptions[j].innerHTML == actualRuleElement.substring(0, actualRuleElement.indexOf(" "))) {
					leftOptions[j] = options[i];
					return;
				}
			}
			alert("Element kann nicht eingefügt werden.");
		}
	}
}

	// removes a rule element from the choosen list
function removeREForConstraint() {
	var nodes = document.leftForm.selected.options;
	var option;
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].selected == true && nodes[i].innerHTML.indexOf(" ") != -1) {
			option = document.createElement('OPTION');
			option.value = nodes[i].value;
			option.innerHTML = nodes[i].innerHTML;
			document.rightForm.selection.appendChild(option);
			nodes[i].value = "";
			nodes[i].innerHTML = nodes[i].innerHTML.substring(0, nodes[i].innerHTML.indexOf(" "));
			return;
		}
	}
	alert("Kein Regelelement in dem linken Feld ausgewählt.");
}

	// selects the rule elements and append the information to the request
function selectRuleElements() {
	var name = $('newName').value;
	if (name == undefined ||name == "") {
		alert("Bitte einen Namen eingeben");
	} else {
		var ruleElements = $('selectedForm').options;
		for (var i = 0; i < ruleElements.length; i++) {
			if (ruleElements[i].innerHTML.indexOf(" ") == -1) {
				alert("Bitte benötigte Regelelemente wählen.");
				return;
			}
		}
		for (var i = 0; i < ruleElements.length; i++) {
			if (ruleElements[i].value != undefined) {
				name = name + "&" + ruleElements[i].value;
			}
		}
		window.name = window.name + "?Constraint=" + 
							$('specification').innerHTML + "(" + name + ")";
		loadConstraintWindow();
	}
}

//=============================================================================================
// functions called from CreateNewConstraintClass.html
//=============================================================================================

	// creates a new constraint class with the selected rule elements
function createNewConstraintClass() {
	var name = $('newName').value;
	var nodes = document.leftForm.selected.options;
	var request = "Constraint=" + name;
	if (nodes.length < 1) {
		alert("Bitte mindestens ein Regelelement auswählen.");
		reutrn;
	}
	for (var i = 0; i < nodes.length; i++) {
		if (nodes[i].innerHTML != undefined && nodes[i].innerHTML != " ") {
			request = request + "&" + nodes[i].innerHTML;
		} 
	}
	if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		xmlhttp.open("GET", "WebOntology.html?" + request, true);
		xmlhttp.send(); 
		console.log("Request send from createNewConstraintClass: " + request);
		xmlhttp.onreadystatechange=function()
		  {
			  if (xmlhttp.readyState==4)
			    {
			    	if (xmlhttp.responseText == "success") {
			    		reloadConstraints();
			    	}
			    }
			}
}


//======================================================================================================
// functions called within this script
//======================================================================================================


function openWindow(site, doc, width, height) {	// open the popup and store the request in the attribute name
	fenster = window.open(site, doc, "width=" + width + ",height=" + height + ",status=yes,scrollbars=yes,resizable=no");
   	fenster.focus();
   	return fenster;
}

	// reloads the constraints from a child page 
function reloadConstraints() {
	while(window.opener.$('constraints').childNodes.length > 0){
			window.opener.$('constraints').removeChild(window.opener.$('constraints').lastChild);
		}
	 if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		xmlhttp.open("GET", "WebOntology.html?Constraint", true);
		xmlhttp.send();  
		xmlhttp.onreadystatechange=function()
		  {
			  if (xmlhttp.readyState==4)
			    {
			    	var constraints = xmlhttp.responseText.split("?");
			    	var ul1 = document.createElement('UL');
			    	ul1.id = "menu4";
			    	for (var j = 0; j < constraints.length; j++) {
			    		if (constraints[j] != "") {
			    			var li1 = document.createElement('LI');
			    			var spanName = document.createElement('SPAN');
			    			var ul2 = document.createElement('UL');
			    				// get the parts of the constraint
			    			var constraintMembers = constraints[j].split("&");
			    			spanName.innerHTML = constraintMembers[0].replace(new RegExp('_', 'g'), ' ');
			    			spanName.id = constraintMembers[0];
			    			spanName.className = "constraintName";
			    			for (var k = 0; k < constraintMembers.length; k++) {
			    				if (constraintMembers[k].indexOf("ruleElements") != -1){
			    					createConstraintRuleElements(constraintMembers[k], ul2);
				    			} else if (constraintMembers[k].indexOf("individuals") != -1) {
				    				if (constraintMembers[k].indexOf(",") != -1) {
				    					createConstraintIndividuals(constraintMembers[k], ul2);
					    			}
				    			}
				    			li1.appendChild(spanName);
							    li1.appendChild(ul2);
							    ul1.appendChild(li1);
			    			}
						}
			    	}
			    	if (li1 != null) {
				    	window.top.$('constraints').appendChild(ul1);
				    	treeMenu_init(window.opener.$('menu4'), '', true);
				    }
				    window.close();
			    	
			    }
		 }
		 
}

	// creates the tree view for the rule elements
function createConstraintRuleElements(constraintMembers, node) {
	var ruleelementLI = document.createElement('LI');
	var ruleelementUL = document.createElement('UL');
	var ruleelementSPAN = document.createElement('SPAN');
	ruleelementSPAN.innerHTML = "Regelelemente";
	ruleelementLI.nodeName = "constraintTree";
	constraintMembers = constraintMembers.substring(
							constraintMembers.indexOf("=") + 1, constraintMembers.length);
	console.log("LoadConstraints Ruleelements: " + constraintMembers);			    											
	if (constraintMembers.indexOf(",") != -1) {
		var ruleelements = constraintMembers.split(",");
		for (var i = 0; i < ruleelements.length; i++) {
			if (ruleelements[i] != "") {
				ruleelements[i] = ruleelements[i].toString().replace(/^(\s|\u00A0)+|(\s|\u00A0)+$/g, ''); 
				var li = document.createElement("li");
			  	var div = document.createElement("div");
			   	var span = document.createElement("span");
			  	span.innerHTML = ruleelements[i];
			   	span.id = ruleelements[i];
			   	span.className = ruleelements[0];
			   	span.style.font = ".9em Times";
			   	li.appendChild(span);
			   	li.className = "";
			   	li.id = "entry";
			   	ruleelementUL.appendChild(li);
			}
		}
		ruleelementLI.appendChild(ruleelementSPAN);
		ruleelementLI.appendChild(ruleelementUL);
		node.appendChild(ruleelementLI);
	}			
}

	// creates the tree view for the individuals
function createConstraintIndividuals(constraintMembers, node) {
	var individualsLI = document.createElement('LI');
	var individualsUL = document.createElement('UL');
	var individualsSPAN = document.createElement('SPAN');
	individualsSPAN.innerHTML = "Individuen";
	individualsLI.nodeName = "constraintTree";
	constraintMembers = constraintMembers.substring(
	constraintMembers.indexOf("=") + 1, constraintMembers.length);
	var individuals = constraintMembers.split(",");
	for (var i = 0; i < individuals.length; i++) {
		if (individuals[i] != "") {
			individuals[i] = individuals[i].toString().replace(/^(\s|\u00A0)+|(\s|\u00A0)+$/g, ''); 
			var li = document.createElement("li");
		  	var div = document.createElement("div");
			var span = document.createElement("span");
		   	span.innerHTML = individuals[i];
		 	span.id = individuals[i];
		  	span.className = individuals[0];
		  	span.style.font = ".9em Times";
		  	li.appendChild(span);
		   	li.className = "";
		  	li.id = "entry";
		   	individualsUL.appendChild(li);
		}
	}
	individualsLI.appendChild(individualsSPAN);
	individualsLI.appendChild(individualsUL);
	node.appendChild(individualsLI);				    				
}

	// sends the request for a new situation
function sendNewSituation() {
	var request = window.name.replace(new RegExp(' ', 'g'), "_");
		if (window.XMLHttpRequest)
			  {// code for IE7+, Firefox, Chrome, Opera, Safari
				  xmlhttp=new XMLHttpRequest();
			  }
			else
			  {// code for IE6, IE5
				  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	          }
			 xmlhttp.open("GET", "WebOntology.html?" + request, true);
			 xmlhttp.send();
			 alert(request);
			 console.log("Request send from sendNewSituation: " + request);
			 xmlhttp.onreadystatechange=function()
				  {
				  	alert(xmlhttp.readyState);
					  if (xmlhttp.readyState==4)
					    {
					   		var text = xmlhttp.responseText;
					   		alert("Response from prepareNewSituation: " + text);
					   		if (text == "succesful") {
								var image = new Image();
								image.src = "bilder/haken_gruen.gif";
								image.id = "img";
								image.value = "success";
								window.top.$("image").appendChild(image);
								var element = document.createElement("span");
								element.id = "successElement";
								element.innerHTML = "" + name + " erfolgreich gespeichert!";
								window.top.$("image").appendChild(element);
					   		} else {
					   			var image = new Image();
								image.src = 'bilder/red_x.gif';
								image.id = "img";
								image.value = "error";
								window.top.$("image").appendChild(image);
								var element = document.createElement("span");
								element.id = "errorElement";
								element.innerHTML = "" + name + " konnte nicht gespeichert werden!";
								window.top.$("image").appendChild(element);
					   		}	
					   		setTimeout('getInformationAboutRule(document.getElementById(\'textOfTheRule\').name)', 1000); 	
					   		reloadConstraints();  
					   		window.opener.close();  	
					    }
				  }
			
}

	// opens a new constraint window
function openConstraintWindow(name, ruleelements) {
	
	if (ruleelements == undefined || ruleelements.length < 1) {
		alert("Keine Regelelemente gefunden.");
		return;
	} else {
		name = name + "&";
		for (var i = 0; i < ruleelements.length; i++) {
			name = name + "," + ruleelements[i];
		}
		fenster = openWindow('CreateNewConstraint.html', name, 900, 400);
	}
	return fenster;
}

	// fetches the situations which belong to the rule 
function getSituations(ruleName) {
	removeAllChilds('situations');
	 if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		xmlhttp.open("GET", "WebOntology.html?Situation=" + ruleName, true);
		xmlhttp.send();  
		console.log("Request send from getSituations: " + "Situations=" + ruleName);
		xmlhttp.onreadystatechange=function()
		  {
			  if (xmlhttp.readyState==4)
			    {
			    	var situations = xmlhttp.responseText.split("?");
			    	var ul1 = document.createElement('UL');
			    	ul1.id = "menu3";
			    	for ( var j = 0; j < situations.length; j++) {
			    		if (situations[j] != "") {
			    			var liSituation = document.createElement('LI');
				    		var spanSituation = document.createElement('SPAN');
				    		var ulSituation = document.createElement('UL');
			    			var situationConstraints = situations[j].split("%");
			    			spanSituation.innerHTML = situationConstraints[0].replace(new RegExp('_', 'g'), " ");
				    		spanSituation.id = situationConstraints[0];
				    		spanSituation.setAttribute('onmouseover', 'selectRelatedRuleElements(\'' + situationConstraints[0] + '\')');
			    			for (var k = 1; k < situationConstraints.length; k++) {
			    				var liConstraint = document.createElement('LI');
			    				var spanConstraint = document.createElement('SPAN');
			    				var ulConstraint = document.createElement('UL');
			    				var situationMembers = situationConstraints[k].split("&");
			    				spanConstraint.innerHTML = situationMembers[0].replace(new RegExp('_', 'g'), " ");
			    				spanConstraint.id = situationMembers[0];
				    			for (var i = 1; i < situationMembers.length; i++) {
				    				var li = document.createElement("li");
								   	var div = document.createElement("div");
								   	var span = document.createElement("span");
								   	span.innerHTML = situationMembers[i].substring(situationMembers[i].indexOf("_") + 1).replace(new RegExp('_', 'g'), " ");
								   	span.id = situationMembers[i];
								   	span.className = situationConstraints[0];
								   	span.style.font = ".9em Times";
								   	li.appendChild(span);
								   	li.className = "";
								   	li.id = "entry";
								   	ulConstraint.appendChild(li);
				    			}
					    		liConstraint.appendChild(spanConstraint);
					    		liConstraint.appendChild(ulConstraint);
					    		ulSituation.appendChild(liConstraint);
			    			}
			    			liSituation.appendChild(spanSituation);
					    	liSituation.appendChild(ulSituation);
					    	ul1.appendChild(liSituation);
						}
			    	}
			    	if (liSituation != null) {
				    	$('situations').appendChild(ul1);
				    	treeMenu_init($('menu3'), '', false);
				    }
				    
			    	loadRelatedRuleElements();
			    }
		 }
	
}

	// removes all rule elements from the list
function removeAllRuleElements() {
	var listOptGroupId = new Array('ActivityOPT', 'ArtifactOPT', 'ProcessOPT', 'RoleOPT', 'PropertyOPT');
	for (var i = 0; i < listOptGroupId.length; i++) {
		removeAllChilds(listOptGroupId[i]);
	}
}

	// fetches all rule elements which belong to the rule
function getRuleElements(rule) {
	 removeAllRuleElements();
	 if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
	 }
	 else
	  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	  }
		
		xmlhttp.open("GET", "WebOntology.html?RuleElements=" + rule, true);
		xmlhttp.send(); 
		console.log("Request send from getRuleElements: " + "RuleElements=" + rule); 
		xmlhttp.onreadystatechange=function()
		  {
			  if (xmlhttp.readyState==4)
			    {
				  var response = xmlhttp.responseText.split("&");
			    	for (var j = 0; j < response.length; j++) {
			    		if (response[j] != "" && response[j].indexOf("?") != -1) {
			    			var subelements = response[j].split("?");
			    			var ruleElement = subelements[0];
			    			for (var i = 1; i < subelements.length; i++) {
			    				
			    				var option = document.createElement("OPTION");
							    option.innerHTML = subelements[i].substring(subelements[i].indexOf("_") + 1).replace(new RegExp('_', 'g'), " ");
							    option.value = subelements[i];
							    var optgroupId = ruleElement + "OPT";
							    option.id = option.value + "_id";
								option.title = "test";
							    $(optgroupId).appendChild(option);
			    			}
						   	
						}
			    	}
			    	getSituations(rule);	
			    }
		 }
	
}
	// retrieves the information about the choosen rule (eg. text, ruleelements, references...)
	function getInformationAboutRule(request){
		removeAllChilds('references');
		if (window.XMLHttpRequest)
		  {// code for IE7+, Firefox, Chrome, Opera, Safari
		  xmlhttp=new XMLHttpRequest();
		  }
		else
		  {// code for IE6, IE5
		  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
		  }
		  xmlhttp.open("GET", "WebOntology.html?" + request, true);
			xmlhttp.send();  
			xmlhttp.onreadystatechange=function()
			  {
				  if (xmlhttp.readyState==4)
				    {
				    	$('ruleName').innerHTML = request.substring(request.indexOf("=") + 1, request.length);
				   		var text = xmlhttp.responseText;
				   		console.log("Make get Request: " + text);
				   		if (text == "error") {
				   			text = "Fehler bei der Verarbeitung des Requests";
				   		}
				   		$('textOfTheRule').innerHTML = text;
				   		$('textOfTheRule').name = request;
				   		$('textOfTheRule').setEditable = false;   
				   		xmlhttp.open("GET", "WebOntology.html?References=" + request.substring(request.indexOf("=") + 1), true);
						xmlhttp.send();
						xmlhttp.onreadystatechange=function()
						  {
						   if (xmlhttp.readyState==4)
						    {
						    	var responseText = xmlhttp.responseText.trim();
						    	console.log("Make get Request References: " + responseText);
						   		var referredRules = responseText.split("?");
						   		if (responseText.length > 3) {
							   		for (var i = 0; i < referredRules.length; i++) {
							   			if (referredRules[i] != "") {
								   			var referredRule = referredRules[i].split("&");
								   			var span = document.createElement('span');
								   			span.id = 'reference';
								   			span.name = referredRule[0] + "=" + referredRule[1];
								   			span.innerHTML = (referredRule[0].italics() + " " + referredRule[1]);
								   			span.setAttribute('onclick', 'getInformationAboutRule(\'' + referredRule[0] + '=' + referredRule[1] + '\')'); 
								   			$('references').appendChild(span);
								   			$('references').appendChild(document.createElement('br'));
							   			}
							   		}
							   	}
						   		getRuleElements(request.substring(request.indexOf("=") + 1, request.length));				
						    }
						 } 
				    }
				  }
	   		
	}
	
	// calculates the indices from the selection in the text
	function selectedText()
	{
	    var userSelection = window.getSelection();
	  	
	    if( userSelection.anchorNode.parentNode.nodeName != userSelection.focusNode.parentNode.nodeName ||
		    userSelection.anchorNode.parentNode.nodeName != "PRE" )
		{
			if (userSelection.focusNode.parentNode.nodeName == "SPAN" && userSelection.anchorNode.parentNode.nodeName == "PRE") {
				
			    var prevNode = userSelection.anchorNode.previousSibling;
				var offset = 0;
			    
			    while( prevNode )
			    {
			        if( prevNode.nodeName == "SPAN" )
			            offset += prevNode.firstChild.length;
			        else    
			            offset += prevNode.length;
			            
			        prevNode = prevNode.previousSibling;
			    }
			    	
			    var range = document.createRange();
			
			    range.setStart( userSelection.anchorNode, userSelection.anchorOffset );
			    range.setEnd( userSelection.focusNode, userSelection.focusOffset );
			    
			    return new Array(range.startOffset + offset, range.startOffset + offset + userSelection.toString().length, userSelection);
				} else {
			    alert("Ungültige Selektion");			
			}			
		} else {
	
			var prevNode = userSelection.anchorNode.previousSibling;
			var offset = 0;
		    
		    while( prevNode )
		    {
		        if( prevNode.nodeName == "SPAN" )
		            offset += prevNode.firstChild.length;
		        else    
		            offset += prevNode.length;
		            
		        prevNode = prevNode.previousSibling;
		    }
		    	
		    var range = document.createRange();
		
		    range.setStart( userSelection.anchorNode, userSelection.anchorOffset );
		    range.setEnd( userSelection.focusNode, userSelection.focusOffset );
		    
		    return new Array(range.startOffset + offset, range.endOffset + offset, userSelection);
	   }
	}
		// adds a given rule element to the choosen list
	function addRE(name) {
	var option = document.createElement('OPTION');
	var counter = 0;
	var options = document.leftForm.selected.options;
	for (var i = 0; i < options.length; i++) {
		if (options[i].innerHTML == name) {
			counter = counter + 1;
		}
	}
	option.innerHTML = name;
	option.id = name + counter;
	document.leftForm.selected.appendChild(option);
}

		// removes all children from a given node
	function removeAllChilds(node){
		while($(node).childNodes.length > 0){
			$(node).removeChild($(node).lastChild);
		}
	}
		// stores the ontology
	function storeOntology(elementName) {
	  	if (window.XMLHttpRequest)
			  {// code for IE7+, Firefox, Chrome, Opera, Safari
				  xmlhttp=new XMLHttpRequest();
			  }
			else
			  {// code for IE6, IE5
				  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	          }
			 xmlhttp.open("GET", "WebOntology.html?storeOntology", true);
			 setTimeout('xmlhttp.send()', 1000); 
			 xmlhttp.onreadystatechange=function()
				  {
					  if (xmlhttp.readyState==4)
					    {
					   		var text = xmlhttp.responseText;
					   		if (text == "succesful") {
								var image = new Image();
								image.src = "bilder/haken_gruen.gif";
								image.id = "img";
								image.value = "success";
								$("image").appendChild(image);
								var element = document.createElement("span");
								element.id = "successElement";
								element.innerHTML = "" + elementName + " erfolgreich gespeichert!";
								$("image").appendChild(element);
					   		} else {
					   			var image = new Image();
								image.src = 'bilder/red_x.gif';
								image.id = "img";
								image.value = "error";
								$("image").appendChild(image);
								var element = document.createElement("span");
								element.id = "errorElement";
								element.innerHTML = "" + elementName + " konnte nicht gespeichert werden!";
								$("image").appendChild(element);
					   		}	
					   		setTimeout('getInformationAboutRule(document.getElementById(\'textOfTheRule\').name)', 1000); 	    	
					    }
				  }
				  // getAllRuleElements();
	  }
	  
//########################################################################################
// functions called from CreateRelatedRuleElements.html
//########################################################################################

	// retrieves the ruleelements from the opening windows
function fillSelectionForms() {
	if (window.opener.document.ruleElementForm) {
		var options = window.opener.document.ruleElementForm.selection.options;
		for (var i = 0; i < options.length; i++) {
			document.relation.firstRuleElement.appendChild(options[i].cloneNode(true));
			document.relation.secondRuleElement.appendChild(options[i].cloneNode(true));
		} 
	} else {
		var text = window.opener.window.name;
		text = text.substring(text.indexOf("ruleelements=&") + 14, text.length);
		var ruleelements = text.split("&");
		for (var i = 0; i < ruleelements.length; i++) {
			var option = document.createElement('OPTION');
			option.value = ruleelements[i];
			option.innerHTML = ruleelements[i].substring(ruleelements[i].indexOf("_") + 1, ruleelements[i].length);
			document.relation.firstRuleElement.appendChild(option.cloneNode(true));
			document.relation.secondRuleElement.appendChild(option.cloneNode(true));
		}
	}
	checkRuleElementRelation();
}

	// adds the seleted ruleelements to the list of relations
function addRelation() {
	var checkedRuleElementRelations = $('storedValue').value.split(",");
	var firstOptions = document.relation.firstRuleElement.options;
	var secondOptions = document.relation.secondRuleElement.options;
	var firstRuleElement; var secondRuleElement; var isValid = false;
	if (firstOptions.length != secondOptions.length) {
		alert("Internal Failure. Wrong number of ruleelements!");
		return;
	}
	for (var i = 0; i < firstOptions.length; i++) {
		if (firstOptions[i].selected == true) {
			firstRuleElement = firstOptions[i];
		} 
		if (secondOptions[i].selected == true) {
			secondRuleElement = secondOptions[i];
		}
	}
	if (firstRuleElement.value == secondRuleElement.value) {
		alert("Bitte zwei unterschiedliche Regelelemente auswählen.");
		return;
	}
	var relations = document.rightForm.selection.options;
	for (var i = 0; i < relations.length; i++) {
		if ((relations[i].value.split("-")[0] == firstRuleElement.value + "&" + secondRuleElement.value) |
			(relations[i].value.split("-")[0] == secondRuleElement.value + "&" + firstRuleElement.value)) {
				alert("Diese Relation ist schon vorhanden.");
				return;
			}
	}
	for (var i = 0; i < checkedRuleElementRelations.length; i++){
		var checkedRelation = checkedRuleElementRelations[i].split("="); // split the entry e.g. Activity&Property=true
		if (checkedRelation[0] == (firstRuleElement.value + "&" + secondRuleElement.value)
			 	&& checkedRelation[1] == "true") {
				isValid = new Boolean(true);
		} else if(checkedRelation[0] == (secondRuleElement.value + "&" + firstRuleElement.value)
		 		&& checkedRelation[1] == "true") {
				isValid = new Boolean(true)
		} 		// if (checkedRelation[0] == (firstRuleElement.value + "&" + secondRuleElement.value)) {
			// alert(checkedRelation + " - " + firstRuleElement.value + "&" + secondRuleElement.value);
		// }	}
	if (!isValid) {
		alert("Leider ist eine Relation zwischen " 
				+ firstRuleElement.value.split("_")[0] 
				+ " und " + secondRuleElement.value.split("_")[0] 
				+ " nicht möglich.");
		return;
	}
	var newRelation = document.createElement('OPTION');
	var newDesription = $('relatedRuleElementsDescription').value;
	newRelation.value = firstRuleElement.value + "&" + secondRuleElement.value + "-" + newDesription;
	// newRelation.innerHTML = firstRuleElement.innerHTML + " &#8594; " + secondRuleElement.innerHTML;	newRelation.innerHTML = newDesription;
	$('relatedRuleElementsDescription').value = "";
	document.rightForm.selection.appendChild(newRelation);
}

	// removes a relation from the right form
function removeRelation() {
	var options = document.rightForm.selection.options;
	for (var i = 0; i < options.length; i++) {
		if (options[i].selected == true) {
			document.rightForm.selection.removeChild(options[i]);
		}
	}
}

	// sends the new relations 
function createNewRelatedRuleElements() {
	var options = document.rightForm.selection.options;
	if (options.length < 1) {
		alert("Keine Relation ausgewählt.");
		return;
	}
	var request = "RelatedRuleElements=";
	for (var i = 0; i < options.length; i++) {
		request = request + options[i].value + "/";
	}
	if (window.XMLHttpRequest)
		  {// code for IE7+, Firefox, Chrome, Opera, Safari
			  xmlhttp=new XMLHttpRequest();
		  }
		else
		  {// code for IE6, IE5
			  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	      }
	xmlhttp.open("GET", "WebOntology.html?" + request, true);
	xmlhttp.send();
	xmlhttp.onreadystatechange=function()
				  {
					  if (xmlhttp.readyState==4)
					    {
					    	window.opener.loadRelatedRuleElements();
							window.close();
						}
				 	}
}

function checkRuleElementRelation() {
	var ruleelements = document.relation.firstRuleElement.options
	var ruleelementsToCheck = "";
	var result;
	for (var i = 0; i < ruleelements.length; i++) {
		for (var j = i + 1; j < ruleelements.length; j++) {
			ruleelementsToCheck = ruleelementsToCheck + "," + ruleelements[i].value + "&" + ruleelements[j].value;
		}
	}
	if (window.XMLHttpRequest)
		  {// code for IE7+, Firefox, Chrome, Opera, Safari
			  xmlhttp=new XMLHttpRequest();
		  }
		else
		  {// code for IE6, IE5
			  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	      }
	xmlhttp.open("GET", "WebOntology.html?checkRuleElements=" + ruleelementsToCheck, true);
	xmlhttp.send();
	xmlhttp.onreadystatechange=function()
				  {
					  if (xmlhttp.readyState==4)
					    {
							var response = xmlhttp.responseText;
							$('storedValue').value = response;
						}
				 	}
}
