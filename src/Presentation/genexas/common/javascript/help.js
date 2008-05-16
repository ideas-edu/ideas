/* 
 * Menubuttons for help, about and a set of rewriting rules.
 * There are default files in common; there may be specific files for each kind of exercise.
 */
function openhelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'rulesButton':
        ($('help')).className='helparea invisible';
		($('about')).className='helparea invisible';
		($('rules')).className='helparea visible';
		break;
    case 'helpButton':
        ($('rules')).className='helparea invisible';
		($('about')).className='helparea invisible';
		($('help')).className='helparea visible';
		break;
    case 'aboutButton':
       ($('help')).className='helparea invisible';
		($('rules')).className='helparea invisible';
		($('about')).className='helparea visible';
	}
}
function closehelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'closerulesButton':
		($('rules')).className='helparea invisible';
		break;
    case 'closehelpButton':
		($('help')).className='helparea invisible';
		break;
    case 'closeaboutButton':
		($('about')).className='helparea invisible';
	}
}
function closeallhelp() {
	($('rules')).className='helparea invisible';
	($('help')).className='helparea invisible';
	($('about')).className='helparea invisible';
}