/**
 *overzicht.js bevat de functies die nodig zijn voor de overzichten voor de docent.
 */
 
/**
 * Voor het toevoegen van een nieuwe combinatie student-variatie
 */
function voegtoe(getal) {
	var form = $("variatie");
	var nieuwbutton = $("nieuw");
	var nweinput = document.createElement("input");
	nweinput.setAttribute('type', "text");
	nweinput.setAttribute('value', "studentnummer");
	nweinput.setAttribute('name', "studentnummer" + getal);
	form.insertBefore(nweinput, nieuwbutton);

	
	var nweselect = document.createElement("select");
	nweselect.setAttribute('name', "url" + getal);
	
	var option1 = document.createElement("option");
	option1.setAttribute('value', "/exas/proplogic/todnf/nl/student.php");
	var option1text = document.createTextNode("Volledige versie");	
	option1.appendChild(option1text);
	nweselect.appendChild(option1);
	
	var option2 = document.createElement("option");
	option2.setAttribute('value', "/exas/proplogic/todnf/nl/zonderstap.php");
	var option2text = document.createTextNode("Zonder Stap");	
	option2.appendChild(option2text);
	nweselect.appendChild(option2);

	var option3 = document.createElement("option");
	option3.setAttribute('value', "/exas/proplogic/todnf/nl/zonderhint.php");
	var option3text = document.createTextNode("Zonder Hint");	
	option3.appendChild(option3text);
	nweselect.appendChild(option3);
	form.insertBefore(nweselect, nieuwbutton);	
	
	var br = document.createElement("br");
	form.insertBefore(br, nieuwbutton);
	//var html = form.innerHtml;
	//if (! html) alert("innerhtml niet gevonden...");
	//alert(html);
	//var nwehtml = "selected=\"selected\">Volledige versie</option><option value=\"/exas/proplogic/todnf/nl/zonderstap.php\">Geen next step</option><option value=\"/exas/proplogic/todnf/nl/zonderhint.php\">Geen hint</option></select><br>";
	//$("variatie").innerhtml = nwehtml;
}

/**
 * De locatie waar de services in de juiste taal staan
  */
function locatie() {
	//return "http://ideas/exas/proplogic/toDNF/nl/";
	return "http://ideas.cs.uu.nl/exas/proplogic/naarDNF/nl/";
}

function $(id) {
	return document.getElementById(id);
}

