<?php
/* 
 * HTTP POST request met datastream als input 
 * de datastream wordt geleverd vanuit clientfeedback.js
 * dat een HTTP POST naar ons stuurt. 
 * De $POST datastroom daarvan is input voor deze functie 
 */
 
 /* Globale variabele: De url van de Happs server */
 $happs = "http://ideas.cs.uu.nl:8001/exas/";
 
function post_it($datastream, $naarurl)
{
	session_start();
	$student = $_SESSION['eastudent'];
	$testbestandnaam = "./studenten/".$student.".txt";
	$testfile = fopen("$testbestandnaam", "a");
	foreach($datastream as $key=>$val) 
    { 
    	$reqbody.= "&"; 
     	$reqbody.= $key."=".urlencode($val); 
     	{
			fwrite($testfile, "Naar de Exercise Assistant: \n\r");
			fwrite($testfile, $key);
			fwrite($testfile, ": ");
   			fwrite($testfile, $val);
   			fwrite($testfile, "\n\r");
   		}
    }
    
     
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $naarurl);
	curl_setopt ($ch, CURLOPT_POST, 1);
	curl_setopt ($ch, CURLOPT_POSTFIELDS, $reqbody);
	curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
	$result = curl_exec ($ch);
	curl_close ($ch);
	
	fwrite($testfile, "Vanuit de Exercise Assistant: \n\r");
	fwrite($testfile, $result);
	fwrite($testfile, "\n\r\\rn");
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
	
	session_start();
	$student = $_SESSION['eastudent'];
	$testbestandnaam = "./studenten/".$student.".txt";
	$testfile = fopen("$testbestandnaam", "a");
	fwrite($testfile, "Gegenereerde vergelijkingen:\n\r");
	fwrite($testfile, $result);
	fwrite($testfile, "\n\r\n\r");
	fclose($testfile);
	curl_close ($ch);
	return $result;
}
/* 
 * HTTP POST request.
 * We sturen een random getal op, met als naam "seed".
 */
function post_seed_params($naarurl, $params)
{
	$seed = rand();
	$encodedseed = urlencode($seed);
   	$reqbody.= "&seed=$encodedseed"; 
	$encodedparams = urlencode($params);
	$reqbody.= "&params=$encodedparams"; 
   	
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $naarurl);
	curl_setopt ($ch, CURLOPT_POST, 1);
	curl_setopt ($ch, CURLOPT_POSTFIELDS, $reqbody);
	curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);	
	$result = curl_exec ($ch);
	
	session_start();
	$student = $_SESSION['eastudent'];
	$testbestandnaam = "./studenten/".$student.".txt";
	$testfile = fopen("$testbestandnaam", "a");
	fwrite($testfile, "Gegenereerde vergelijkingen:\n\r");
	fwrite($testfile, $result);
	fwrite($testfile, "\n\r\n\r");
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
	
	$testfile = fopen(($_SESSION["eastudent"]).".txt", "wt");
	fwrite($testfile, "Gegenereerde vergelijkingen:\n");
	fwrite($testfile, $result);
	fwrite($testfile, "\n");
	fclose($testfile);
	curl_close ($ch); 
   return $result;
}
?>