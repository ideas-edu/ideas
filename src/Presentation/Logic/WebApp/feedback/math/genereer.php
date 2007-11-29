<?php
 include_once("../common/post-get.php");
/*
 * Wordt aangeroepen wanneer de gebruiker een nieuwe
 * set van formules nodig heeft.
 */
//	echo post_seed($seed, "http://localhost:8000/generator/");
	printf(post_seed("http://localhost:8000/math/generator/"));
?>