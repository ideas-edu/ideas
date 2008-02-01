/* 
 * Wordt aangeroepen wanneer een menuknop wordt ingedrukt
 * Eerste parameter: het venster dat zichtbaar moet worden.
 * Tweede parameter: het venster dat moet verdwijnen als het oppen stond.
 */
function menuhelp(verschijn)
{
	switch (verschijn) {
    case 'regels':
        $('help').className='helpgebied onzichtbaar';
        $('about').className='helpgebied onzichtbaar';
    case 'help':
        $('regels').className='helpgebied onzichtbaar';
        $('about').className='helpgebied onzichtbaar';
    case 'about':
        $('help').className='helpgebied onzichtbaar';
        $('regels').className='helpgebied onzichtbaar';
    default:
        ;
    }	
    $(verschijn).className='helpgebied zichtbaar';
}

/* 
 * Wordt aangeroepen wanneer de Sluit-menu-knop wordt ingedrukt
 */
function sluitmenuhelp(id)
{
	$(id).className='onzichtbaar';
}

/* 
 * Wordt aangeroepen wanneer een werkveldknop wordt ingedrukt
 */
function help(id)
{
	$(id).className='zichtbaar minibutton';
}

/* 
 * Wordt aangeroepen wanneer een werkveldknop wordt ingedrukt
 */
function sluithelp(id)
{
	$(id).className='onzichtbaar  minibutton';
}