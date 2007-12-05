<?php
/* 
 * HTTP POST request met datastream als input 
 * de datastream wordt geleverd vanuit clientfeedback.js
 * dat een HTTP POST naar ons stuurt. 
 * De $POST datastroom daarvan is input voor deze functie 
 */
 
 /* Globale variabele: De url van de Happs server */
 $happs = "http://localhost:8001/exas/";
 
function post_it($datastream, $naarurl)
{		
	$testfile = fopen("test.txt", "a");
	foreach($datastream as $key=>$val) 
    { 
    	$reqbody.= "&"; 
     	$reqbody.= $key."=".urlencode($val); 
     	{
			fwrite($testfile, "Naar de Exercise Assistant: \n");
			fwrite($testfile, "$naarurl \n");
   			fwrite($testfile, $val);
   			fwrite($testfile, "\n\n\n");
   		}
    }
    
     
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $naarurl);
	curl_setopt ($ch, CURLOPT_POST, 1);
	curl_setopt ($ch, CURLOPT_POSTFIELDS, $reqbody);
	curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
	$result = curl_exec ($ch);
	curl_close ($ch);
	
	fwrite($testfile, "Vanuit de Exercise Assistant: \n");
	fwrite($testfile, $result);
	fwrite($testfile, "\n\n\n");
	fclose($testfile);
	return $result;
}
/* 
 * HTTP POST request.
 * We sturen een random getal op, met als naam "seed".
 */
function post_seed($naarurl)
{
	$seed = rand();
	$encodedseed = urlencode($seed);
   	$reqbody.= "&seed=$encodedseed"; 
   	
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $naarurl);
	curl_setopt ($ch, CURLOPT_POST, 1);
	curl_setopt ($ch, CURLOPT_POSTFIELDS, $reqbody);
	curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);	
	$result = curl_exec ($ch);
	
	$testfile = fopen("test.txt", "a");
	fwrite($testfile, "Gegenereerde vergelijkingen:\n");
	fwrite($testfile, $result);
	fwrite($testfile, "\n\n\n");
	fclose($testfile);
	
	curl_close ($ch);
	return $result;
}

/*
 * Versturen van een HTTP-GET request
 */
function get_it($naarurl){
	session_start();
 
	$ch = curl_init(); 
	curl_setopt ($ch, CURLOPT_URL, $naarurl); 
	curl_setopt ($ch, CURLOPT_HEADER, 0); 
	curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1); 
	$result = curl_exec ($ch); 
	
	$testfile = fopen(($_SESSION["studentnummer"]).".txt", "wt");
	fwrite($testfile, "Gegenereerde vergelijkingen:\n");
	fwrite($testfile, $result);
	fwrite($testfile, "\n");
	fclose($testfile);
	curl_close ($ch); 
   return $result;
}
?>