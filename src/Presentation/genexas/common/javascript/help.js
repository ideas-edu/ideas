/* 
 * Wordt aangeroepen wanneer een menuknop wordt ingedrukt
 * Eerste parameter: het venster dat zichtbaar moet worden.
 * Tweede parameter: het venster dat moet verdwijnen als het oppen stond.
 */
function menuhelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'regelsButton':
        (get('help')).className='helpgebied onzichtbaar';
		(get('about')).className='helpgebied onzichtbaar';
		(get('regels')).className='helpgebied zichtbaar';
		break;
    case 'helpButton':
        (get('regels')).className='helpgebied onzichtbaar';
		(get('about')).className='helpgebied onzichtbaar';
		(get('help')).className='helpgebied zichtbaar';
		break;
    case 'aboutButton':
       (get('help')).className='helpgebied onzichtbaar';
		(get('regels')).className='helpgebied onzichtbaar';
		(get('about')).className='helpgebied zichtbaar';
	}
}
function sluitmenuhelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'sluitmenuregelsButton':
		(get('regels')).className='helpgebied onzichtbaar';
		break;
    case 'sluitmenuhelpButton':
		(get('help')).className='helpgebied onzichtbaar';
		break;
    case 'sluitmenuaboutButton':
		(get('about')).className='helpgebied onzichtbaar';
	}
}
